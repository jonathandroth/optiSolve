
##################################################################
# This function transforms an optimization problem (op) into     #
# an equivalent simpler optimization problem.                    #
# -> Constant terms d are merged with constraint thresholds val. #
# -> Maximization problems with linear objective are converted   #
#    to minimization problems                                    #
##################################################################

"simplify" <- function(op){
  if(class(op$f)=="linFun"){
    op$f$d <- 0*op$f$d
    if(op$max){
      op$f$a <- - op$f$a
      op$max <- FALSE
    }
    op$f$a <- 0.1*(op$f$a - mean(op$f$a))/sd(op$f$a)
  }
  if(class(op$f)=="quadFun"){
    op$f$d <- 0*op$f$d
  }
  if(!is.null(op$lc)){
    op$lc$val <- op$lc$val - op$lc$d
    op$lc$d   <- 0*op$lc$d
    for(i in 1:nrow(op$lc$A)){
      if((!all(op$lc$A[i,] %in% c(0,1))) && (op$lc$dir[i]!="==")){
        lambda <- 1/max(abs(op$lc$A[i,]))
        op$lc$A[i,]  <- lambda*op$lc$A[i,]
        op$lc$val[i] <- lambda*op$lc$val[i]
      }
    }
  }
  Seq <- seq_along(op$qc)
  for(i in Seq){
    op$qc[[i]]$val <- op$qc[[i]]$val - op$qc[[i]]$d 
    op$qc[[i]]$d   <- 0*op$qc[[i]]$d
  }
  op
}