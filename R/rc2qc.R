
##################################################################
# This function transforms an optimization problem (op) into     #
# an equivalent other optimization problem.                      #
# -> Rational constraints are replaced by quadratic conatraints. #
##################################################################

"rc2qc"<-function(op, quiet=FALSE){

  if((is.null(op$lc) || (!any(op$lc$dir == "=="))) && length(op$rc)>0){
    stop("If a rational function is constrained, then an equality constraint for the mean is needed.")
  }
  
  ## Find out what sum(x) is ##
  
  eq <- op$lc$dir == "=="
  invA <- MASS::ginv(op$lc$A[eq, , drop=FALSE])
  diff <- (op$lc$val-op$lc$d)[eq]
  sumx <- sum(invA%*%diff)
  if(abs(sumx+sum(op$x[!is.na(op$x)])-1)<0.00001){
    sumx <- 1-sum(op$x[!is.na(op$x)])
  }else{
    if(!quiet){cat(paste0("It is assumed that sum(x)=", round(sumx,5),"\n"))}
    if(!quiet){cat("Make sure that an equality constraint for the sum is included.\n")}
  }
  
  ## Convert rc to qc ##
  Seq <- seq_along(op$rc)
  for(i in Seq){
    q <- op$rc[[i]]$val
    N <- nrow(op$rc[[i]]$Q1)
    
    lambda <- 0.1/(q*max(max(op$rc[[i]]$Q1), max(op$rc[[i]]$Q2)))
    op$rc[[i]]$Q1 <- lambda*op$rc[[i]]$Q1
    op$rc[[i]]$Q2 <- lambda*op$rc[[i]]$Q2
    op$rc[[i]]$a1 <- lambda*op$rc[[i]]$a1
    op$rc[[i]]$a2 <- lambda*op$rc[[i]]$a2
    op$rc[[i]]$d1 <- lambda*op$rc[[i]]$d1
    op$rc[[i]]$d2 <- lambda*op$rc[[i]]$d2
    
    con <- list(Q   = op$rc[[i]]$Q1 + matrix(1, N, N) - q*op$rc[[i]]$Q2, 
                a   = op$rc[[i]]$a1 - q*op$rc[[i]]$a2, 
                d   = op$rc[[i]]$d1 - q*op$rc[[i]]$d2, 
                dir = op$rc[[i]]$dir, 
                val = sumx^2, 
                id  = op$rc[[i]]$id)
    class(con) <- "quadCon"
    
    op$qc[[length(op$qc)+1]] <- con
  }
  
  op$rc <- list()
  op
}