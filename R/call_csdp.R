
"call_csdp" <- function(op, X=NULL, opt, quiet=FALSE){
  if(class(op$f)=="ratioFun"){stop("Solver not suitable.\n")}
  
  if(!op$madeDefinite){
    op <- asdefinite(op, quiet=quiet)
  }
  
  ### Define optimization parameters ##################
  if(! "axtol"       %in% names(opt)){opt$axtol       = 1e-08}
  if(! "atytol"      %in% names(opt)){opt$atytol      = 1e-08}
  if(! "objtol"      %in% names(opt)){opt$objtol      = 1e-05}
  if(! "pinftol"     %in% names(opt)){opt$pinftol     = 1e+20}
  if(! "dinftol"     %in% names(opt)){opt$dinftol     = 1e+20}
  if(! "maxiter"     %in% names(opt)){opt$maxiter     = 100}
  if(! "minstepfrac" %in% names(opt)){opt$minstepfrac = 0.8}
  if(! "maxstepfrac" %in% names(opt)){opt$maxstepfrac = 0.97}
  if(! "minstepp"    %in% names(opt)){opt$minstepp    = 1e-20}
  if(! "minstepd"    %in% names(opt)){opt$minstepd    = 1e-20}
  if(! "usexzgap"    %in% names(opt)){opt$usexzgap    = 1}
  if(! "tweakgap"    %in% names(opt)){opt$tweakgap    = 0}
  if(! "affine"      %in% names(opt)){opt$affine      = 0}
  if(! "printlevel"  %in% names(opt)){opt$printlevel  = 1*(!quiet)} 
  if(! "perturbobj"  %in% names(opt)){opt$perturbobj  = 1}
  if(! "fastmode"    %in% names(opt)){opt$fastmode    = 0}
  params.opt <- c("axtol", "atytol", "objtol", "pinftol", "dinftol", "maxiter", "minstepfrac", "maxstepfrac", "minstepp", "minstepd", "usexzgap", "tweakgap", "affine", "printlevel", "perturbobj", "fastmode")
  opt<-opt[intersect(names(opt), params.opt )]
  
  if(!quiet){
    cat("\n")
    cat("Using solver 'csdp' with parameters: \n")
    print(data.frame(Value=as.character(rapply(opt,c)),row.names=names(rapply(opt,c))))
    cat("\n")
  }
  
  ### Add bounds to lc ###################################
  if(length(op$lb$val)>0 || length(op$ub$val)>0){
    op <- bounds2lc(op)
  }
  
  ### Separate equality and inequality constraints #######
  op <- splitlc(op)

  ### Convert op into semidefinite programming problem #######
  
  N <- length(op$id)
  nblocks <- length(op$qc)+(!is.null(op$eqlc$A))+(!is.null(op$inlc$A))+(!is.null(op$f$Q))
  Y <- vector("list", N+(!is.null(op$f$Q)))
  C <- vector("list", nblocks)
  K <- list(type=rep('', nblocks), size=rep(0,nblocks))
  is <- 1
  if(!is.null(op$eqlc$A)){
    C[[is]]    <- c(c(op$eqlc$val),c(-op$eqlc$val));
    K$type[is] <- "l"; 
    K$size[is] <- length(C[[is]]); 
    is <- is+1
    }
  if(!is.null(op$inlc$A)){
    C[[is]]    <- c(-op$inlc$val); 
    K$type[is] <- "l"; 
    K$size[is] <- length(C[[is]]); 
    is <- is+1
    }
  for(j in seq_along(op$qc)){
    C[[is]]    <- as(Matrix::bdiag((-1)*ginv(op$qc[[j]]$Q), (-1)*(op$qc[[j]]$val)),"matrix")
    K$type[is] <- "s"; 
    K$size[is] <- N+1;
    is <- is+1
  }
  if(!is.null(op$f$Q)){
    C[[is]]    <- as(Matrix::bdiag((-1)*ginv(op$f$Q),0),"matrix")
    K$type[is] <- "s"; 
    K$size[is] <- N+1;
    is <- is+1 
  }
  for(i in 1:N){
    Y[[i]] <- vector("list", nblocks)
    is <- 1
    if(!is.null(op$eqlc$A)){
      Y[[c(i,is)]] <- c(op$eqlc$A[,i],-op$eqlc$A[,i]); 
      is <- is+1
      }
    if(!is.null(op$inlc$A)){
      Y[[c(i,is)]] <- -op$inlc$A[,i]; 
      is <- is+1
      }
    for(j in seq_along(op$qc)){
      Y[[c(i,is)]] <- simple_triplet_sym_matrix(i=c(i,N+1),j=c(N+1,N+1),v=c(1,-2*0.5*op$qc[[j]]$a[i]))
      is <- is+1
    }
    if(!is.null(op$f$Q)){
      Y[[c(i,is)]] <- simple_triplet_sym_matrix(i=c(i,N+1),j=c(N+1,N+1),v=c(1,-op$f$a[i]))
      is <- is+1
    }
  }
  if(!is.null(op$f$Q)){
    Y[[N+1]] <- vector("list", nblocks)
    is <- 1
    if(!is.null(op$eqlc$A)){
      Y[[c(N+1,is)]] <- 0*c(op$eqlc$A[,1],-op$eqlc$A[,1]); 
      is <- is+1
      }
    if(!is.null(op$inlc$A)){
      Y[[c(N+1,is)]] <- 0*op$inlc$A[,1]; 
      is <- is+1
      }
    for(j in seq_along(op$qc)){
      Y[[c(N+1,is)]] <- simple_triplet_sym_matrix(i=c(1,N+1),j=c(N+1,1),v=c(0,0))
      is <- is+1
    }
    Y[[c(N+1,is)]] <- simple_triplet_sym_matrix(i=c(N+1),j=c(N+1),v=c(1))
    is <- is+1
  }
  if(is.null(op$f$Q)){
    B <- 0.5*op$f$a
  }else{
    B <- c(rep(0,N),1)
  }
  
  ### Solve the optimization problem ######################
  res <- Rcsdp::csdp(C, Y, B, K, control= opt)
  
  if(res$status==0){status <- "Success"}
  if(res$status==1){status <- "Success. Problem is primal infeasible"}
  if(res$status==2){status <- "Success. Problem is dual infeasible"}
  if(res$status==3){status <- "Partial Success. Solution found but full accuracy was not achieved"}
  if(res$status==4){status <- "Failure. Maximum number of iterations reached"}
  if(res$status==5){status <- "Failure. Stuck at edge of primal feasibility"}
  if(res$status==6){status <- "Failure. Stuch at edge of dual infeasibility"}
  if(res$status==7){status <- "Failure. Lack of progress"}
  if(res$status==8){status <- "Failure. X or Z (or Newton system O) is singular"}
  if(res$status==9){status <- "Failure. Detected NaN or Inf values"}
  
  res <- list(x=setNames(res$y[1:N], op$id), solver="csdp", status=status)
  return(res)
}