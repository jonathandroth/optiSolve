
"call_slsqp" <- function(op, X=NULL, opt, quiet=FALSE){

  ### Define optimization parameters ##################

  if(!("nl.info"  %in% names(opt))){opt$nl.info  <- FALSE}
  if(!("stopval"  %in% names(opt))){opt$stopval  <- -Inf}
  if(!("xtol_rel" %in% names(opt))){opt$xtol_rel <- 1e-06}
  if(!("maxeval"  %in% names(opt))){opt$maxeval  <- 1000}
  if(!("ftol_rel" %in% names(opt))){opt$ftol_rel <- 0.0}
  if(!("ftol_abs" %in% names(opt))){opt$ftol_abs <- 0.0}
  if(!("check_derivatives" %in% names(opt))){opt$check_derivatives <- FALSE}
  optlist <- opt[intersect(names(opt), c("stopval", "xtol_rel", "maxeval", "ftol_rel", "ftol_abs", "check_derivatives"))]
  
  if(!quiet){
    cat("\n")
    cat("Using solver 'slsqp' with parameters: \n")
    print(data.frame(Value=as.character(rapply(opt,c)),row.names=names(rapply(opt,c))))
    cat("\n")
  }
  
  ### Separate equality and inequality constraints #######
  op <- splitlc(op)
  
  ### Transform  objective function and constraints to functions ##
  op <- f2fun(op)      # Objective function
  op <- incon2fun(op, leq=FALSE)  # Inequality constraints
  op <- eqcon2fun(op)  # Equality constraints
  
  ### Get a starting value ## 

  if(is.null(X)){X <- getX(op)}

  ### Solve the optimization problem ######################

  
  res <- nloptr::slsqp(x0=X, fn=op$f$f0, gr=op$f$g0, lower=op$lb$val, upper=op$ub$val, hin=op$infun$f0, hinjac=op$infun$g0, heq=op$eqfun$f0, heqjac=op$eqfun$g0, nl.info=opt$nl.info, control=optlist)

  status <- ifelse(res$convergence>1, "successful completion", res$message)
  res <- list(x=setNames(res$par, op$id), solver="slsqp", status=status)
  return(res)
}