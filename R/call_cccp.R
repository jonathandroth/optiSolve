
"call_cccp" <- function(op, X=NULL, opt, quiet=FALSE){
  if(class(op$f)=="ratioFun" && op$max){
    stop("Rational functions can only be minimized with solver='cccp'.\n")
  }
  if(class(op$f)=="quadFun" && op$max){
    stop("Quadratic functions can only be minimized with solver='cccp'.\n")
  }
  
  ### Define optimization parameters ##################
  if(!("abstol"  %in% names(opt))){opt$abstol    = (1e-06)*(10^length(op$qc)) }
  if(!("feastol" %in% names(opt))){opt$feastol   = 1e-05}
  if(!("trace"   %in% names(opt))){opt$trace     = !quiet}
  if(!("stepadj" %in% names(opt))){opt$stepadj   = ifelse(class(op$f)=="ratioFun",0.40, 0.90)}
  if(!("maxiters" %in% names(opt))){opt$maxiters = 100L}
  if(!("reltol"   %in% names(opt))){opt$reltol   = 1e-06}
  if(!("beta"     %in% names(opt))){opt$beta     = 0.5}
  storage.mode(opt$maxiters) <- "integer"
  
  params.opt <- c("maxiters", "abstol", "reltol", "feastol", "stepadj", "beta", "trace")
  opt    <- opt[intersect(names(opt), params.opt)]
  optctrl<- do.call(cccp::ctrl, opt)
  
  if(!quiet){
    cat("\n")
    cat("Using solver 'cccp' with parameters: \n")
    print(data.frame(Value=as.character(rapply(opt,c)),row.names=names(rapply(opt,c))))
    cat("\n")
  }
  ### Find a starting value within the domain if f  ########
  ### is rational and adjust linear equality constraints ###
  
  if(is.null(X) && (class(op$f)=="ratioFun")){
    X  <- getX(op)
    eq <- op$lc$dir=="=="
    if(any(eq)){
      op$lc$val[eq] <- op$lc$A[eq,,drop=FALSE] %*% X
    }
  }
  
  
  ### Add bounds to lc ###################################
  if(length(op$lb$val)>0 || length(op$ub$val)>0){
    op <- bounds2lc(op)
  }
  
  ### Separate equality and inequality constraints #######
  op <- splitlc(op)
  
  ### Transform qc to second order cone constraints ######
  if(length(op$qc)>0){
    op <- qc2socc(op, quiet=quiet)
  }
  

    
  ### Transform rational objective function to function ##
  if(class(op$f)=="ratioFun"){
    op <- f2fun(op)
  }
  if(class(op$f)=="linFun"){
    P <- NULL
    q <- op$f$a
  }
  if(class(op$f)=="quadFun"){
    P <- 2*op$f$Q
    q <- op$f$a
  }  
  
  ### Define parameter cList #############################
  cList <- NULL
  soccNumber <- length(op$socc)
  ineqNumber <- !is.null(op$inlc)
  if(soccNumber+ineqNumber>0){
    cList <- vector("list", soccNumber+ineqNumber)
    if(ineqNumber>0){
      cList[[1]] <- nnoc(G=op$inlc$A, h=op$inlc$val)
    }
    for(i in seq_along(op$socc)){
      cList[[ineqNumber+i]] <- do.call(socc, op$socc[[i]][c("F", "g", "d", "f")])
    }
  }
 
  ### Solve the optimization problem ######################
  if(class(op$f)=="Fun"){
    suppressWarnings(res <- cccp(f0=op$f$f0, g0=op$f$g0, h0=op$f$h0, x0=X, A=op$eqlc$A, b=op$eqlc$val, cList=cList, optctrl=optctrl))
  }else{
    suppressWarnings(res <- cccp(P=P, q=c(q), A=op$eqlc$A, b=op$eqlc$val, cList=cList, optctrl=optctrl))
  }
  
  if(length(getx(res))==0){
    x <- rep(NA, length(op$id))
  }else{
    x <- c(getx(res))
  }
  x <- setNames(x, op$id)
  
  res <- list(x=x, solver="cccp", status=res$status)
  res
}
