
"call_cccp2" <- function(op, X=NULL, opt, quiet=FALSE){
  if(class(op$f)=="ratioFun" && op$max){
    stop("Rational functions can only be minimized with solver='cccp2'.\n")
  }
  if(class(op$f)=="quadFun" && op$max){
    stop("Quadratic functions can only be minimized with solver='cccp2'.\n")
  }
  if(length(op$qc)==0){
    stop("There are no quadratic or rational constraints, so use solver='cccp' instead.\n")
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
    cat("Using solver 'cccp2' with parameters: \n")
    print(data.frame(Value=as.character(rapply(opt,c)),row.names=names(rapply(opt,c))))
    cat("\n")
    }
  
  ### Add bounds to lc ###################################
  if(length(op$lb$val)>0 || length(op$ub$val)>0){
    op <- bounds2lc(op)
  }

  ### Separate equality and inequality constraints #######
  op <- splitlc(op)
  
  ### Convert linear inequality constraints to functions ###
  op <- incon2fun(op, multiDim=FALSE, withLinCon=FALSE)

  ### Transform rational objective function to function ##
  if(class(op$f)=="ratioFun"){
    op <- f2fun(op)
  }
  
  ### Define parameter cList #############################
  if(is.null(op$inlc)){
    cList <- NULL
  }else{
    nnoc1 <- nnoc(G=op$inlc$A, h=op$inlc$val)  
    cList <- list(nnoc1)
  }
  
  ### Get starting values within the domain ### 
  ### of the nonlinear constraints ############ 

  gc()
  myf <- list()
  myg <- list()
  myh <- list()
  for(i in seq_along(op$infun$f0)){
    op$infun$val[[i]]<-op$infun$val[[i]]-0.001
  }
  
  for(i in seq_along(op$infun$f0)){
    if(is.null(X) || op$infun$f0[[i]](X)>=0){
      
      X <- getx(cccp(   P      = op$infun$Q[[i]], 
                        q      = op$infun$a[[i]], 
                        A      = op$eqlc$A, 
                        b      = op$eqlc$val, 
                        cList  = cList, 
                        nlfList= myf, 
                        nlgList= myg, 
                        nlhList= myh, 
                        x0     = X, 
                        optctrl=optctrl))[,1]
      
      if(length(X)==0){X <- rep(NA, length(op$id))}
      
      if(!is.null(op$eqlc)){op$eqlc$val <- c(op$eqlc$A%*%X)}
      }
    myf[[i]] <- op$infun$f0[[i]]
    myg[[i]] <- op$infun$g0[[i]]
    myh[[i]] <- op$infun$h0[[i]]
    gc()
    
    if(op$infun$f0[[i]](X)>0){
      cat("No solution exists.\n");
      X <- setNames(X, op$id) 
      return(list(x=X, solver="cccp2", status="No solution exists."))
    }
  }
  for(i in seq_along(op$infun$f0)){
    op$infun$val[[i]] <- op$infun$val[[i]]+0.001
  }
  gc()

  ### Solve the optimization problem ######################
  if(class(op$f)=="Fun"){
    res <- cccp(f0=op$f$f0, g0=op$f$g0, h0=op$f$h0, x0=X, A=op$eqlc$A, b=op$eqlc$val, cList=cList, nlfList=op$infun$f0, nlgList=op$infun$g0, nlhList=op$infun$h0, optctrl=optctrl)
  }else{
    res <- cccp(P=op$f$Q, q=0.5*op$f$a,             x0=X, A=op$eqlc$A, b=op$eqlc$val, cList=cList, nlfList=op$infun$f0, nlgList=op$infun$g0, nlhList=op$infun$h0, optctrl=optctrl) 
  }
  
  if(length(getx(res))==0){
    x <- rep(NA, length(op$id))
  }else{
    x <- c(getx(res))
  }
  x <- setNames(x, op$id) 

  res <- list(x=x, solver="cccp2", status=res$status)
  res
}
