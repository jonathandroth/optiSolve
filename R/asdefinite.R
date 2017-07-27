
##################################################################
# This function approximates an optimization problem (op) by     #
# an other convex optimization problem (if possible).            #
# -> Matrices involved in quadratic functions and constraints are#
#    approximated by positive definite matrices.                 #
##################################################################


"asdefinite" <- function(op, quiet=FALSE){
  if(class(op$f)=="quadFun"){
    op$f$Q <- asDefinite(op$f$Q, quiet=quiet)
  }
  if(class(op$f)=="ratioFun"){
    #op$f$Q1 <- asDefinite(op$f$Q1, quiet=quiet) #necessary?
    #op$f$Q2 <- asDefinite(op$f$Q2, quiet=quiet) #necessary?
  }
  Seq <- seq_along(op$qc)
  for(i in Seq){
    op$qc[[i]]$Q <- asDefinite(op$qc[[i]]$Q, quiet=quiet)
  }
  op$madeDefinite <- TRUE
  op
}

"asDefinite" <- function(Q, quiet=FALSE){
  changed <- FALSE
  oldQ    <- Q
  diagQ   <- diag(Q)
  epsilon <- max(abs(diagQ))*0.0001
  
  if(any(diagQ<epsilon)){
    changed <- TRUE
    diagQ[diagQ<epsilon] <- epsilon
    diag(Q) <- diagQ
    }
  
  x<-base::eigen(Q, only.values=TRUE, EISPACK=TRUE)$values
  
  if(min(x)<epsilon){
    x <- base::eigen(Q, only.values=FALSE, EISPACK=TRUE)
    v <- x$values
    v[v<epsilon] <- epsilon
    Q <- x$vectors%*%diag(v)%*%t(x$vectors)
    changed <- TRUE
  }
  if(changed){
    reldist <- shapes::distcov(oldQ, Q, method="Euclidean")/shapes::distcov(oldQ, 0*oldQ, method="Euclidean")
    if(!quiet){cat("A square matrix was approximated by a positive\n")}
    if(!quiet){cat(paste0("  definite matrix with relative distance ", round(reldist, 4), ".\n"))}   
  }
  Q
}
