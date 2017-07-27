
"call_alabama" <- function(op, X=NULL, opt, quiet=FALSE){
  
  ### Define optimization parameters ##################

  params.outer  <- c("lam0", "sig0", "eps", "itmax", "ilack.max", "trace", "method", "NMinit", "i.scale", "e.scale", "kkt2.check")
  params.optim  <- c("trace", "fnscale", "parscale", "ndeps", "maxit", "abstol", "reltol", "alpha", "beta", "gamma", "REPORT", "type", "lmm", "factr", "pgtol", "temp", "tmax")
  if(! "trace"    %in% names(opt)){opt$trace    <- !quiet}
  optctrl.outer <- opt[intersect(names(opt), params.outer)]
  optctrl.optim <- opt[intersect(names(opt), params.optim)]
  if(! "lam0"     %in% names(opt)){opt$lam0     <- "default"}
  if(! "sig0"     %in% names(opt)){opt$sig0     <- "default"}
  if(! "eps"      %in% names(opt)){opt$eps      <- "default"}
  if(! "itmax"    %in% names(opt)){opt$itmax    <- "default"}
  if(! "ilack.max"%in% names(opt)){opt$ilack.max<- "default"}
  if(! "method"   %in% names(opt)){opt$method   <- "default"}
  if(! "NMinit"   %in% names(opt)){opt$NMinit   <- "default"}
  if(! "i.scale"   %in% names(opt)){opt$i.scale   <- "default"}
  if(! "e.scale"   %in% names(opt)){opt$e.scale   <- "default"}
  if(! "kkt2.check"   %in% names(opt)){opt$kkt2.check   <- "default"}
  if(! "fnscale"  %in% names(opt)){opt$fnscale  <- "default"}
  if(! "parscale" %in% names(opt)){opt$parscale <- "default"}
  if(! "ndeps"    %in% names(opt)){opt$ndeps    <- "default"}
  if(! "maxit"    %in% names(opt)){opt$maxit    <- "default"}
  if(! "abstol"   %in% names(opt)){opt$abstol   <- "default"}
  if(! "reltol"   %in% names(opt)){opt$reltol   <- "default"}
  if(! "alpha"    %in% names(opt)){opt$alpha    <- "default"}
  if(! "beta"     %in% names(opt)){opt$beta     <- "default"}
  if(! "gamma"    %in% names(opt)){opt$gamma    <- "default"}
  if(! "REPORT"   %in% names(opt)){opt$REPORT   <- "default"}
  if(! "type"     %in% names(opt)){opt$type     <- "default"}
  if(! "lmm"      %in% names(opt)){opt$lmm      <- "default"}
  if(! "factr"    %in% names(opt)){opt$factr    <- "default"}
  if(! "pgtol"    %in% names(opt)){opt$pgtol    <- "default"}
  if(! "temp"     %in% names(opt)){opt$temp     <- "default"}
  if(! "tmax"     %in% names(opt)){opt$tmax     <- "default"}
  opt <- opt[unique(c(params.outer, params.optim))]
  
  if(!quiet){
    cat("\n")
    cat("Using solver 'alabama' with parameters: \n")
    print(data.frame(Value=as.character(rapply(opt,c)),row.names=names(rapply(opt,c))))
    cat("\n")
  }
  ### Add bounds to lc ###################################
  if(length(op$lb$val)>0 || length(op$ub$val)>0){
    op <- bounds2lc(op)
  }
  
  ### Separate equality and inequality constraints #######
  op <- splitlc(op)
  
  ### Transform  objective function and constraints to functions ##
  op <- f2fun(op)                 # Objective function
  op <- incon2fun(op, leq=FALSE)  # Inequality constraints
  op <- eqcon2fun(op)             # Equality constraints
  
  ### Get a starting value ## 
  if(is.null(X)){X <- getX(op)}

  ### Solve the optimization problem ######################
  if(is.null(op$eqfun$f0) && !is.null(op$infun$f0)){
    res <- alabama::auglag(par=X, fn=op$f$f0, gr=op$f$g0, hin=op$infun$f0, hin.jac=op$infun$g0, control.outer=optctrl.outer, control.optim=optctrl.optim)
  }
  if(!is.null(op$eqfun$f0) && is.null(op$infun$f0)){
    res <- alabama::auglag(par=X, fn=op$f$f0, gr=op$f$g0, heq=op$eqfun$f0, heq.jac=op$eqfun$g0, control.outer=optctrl.outer, control.optim=optctrl.optim)
  }
  if(!is.null(op$eqfun$f0) && !is.null(op$infun$f0)){
    res <- alabama::auglag(par=X, fn=op$f$f0, gr=op$f$g0, hin=op$infun$f0, hin.jac=op$infun$g0, heq=op$eqfun$f0, heq.jac=op$eqfun$g0, control.outer=optctrl.outer, control.optim=optctrl.optim)
  }
  if(is.null(op$eqfun$f0) && is.null(op$infun$f0)){
    stop("Unconstrained optimization problems cannot be solved with alabama.\n")
  }
  
  status <- ifelse(res$convergence==0, "successful convergence", "Failure to converge")
  res <- list(x=setNames(res$par, op$id), solver="alabama", status=status)
  return(res)
}