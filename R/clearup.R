
##################################################################
# This function transforms an optimization problem (op) into     #
# an equivalent simpler optimization problem.                    #
# -> Constraints which are not used are removed.                 #
##################################################################


"clearup" <- function(op){
  if(!is.null(op$lc)){
    if(any(op$lc$use)){
      op$lc$A   <- op$lc$A[op$lc$use,,drop=FALSE]
      op$lc$d   <- op$lc$d[op$lc$use]
      op$lc$dir <- op$lc$dir[op$lc$use]
      op$lc$val <- op$lc$val[op$lc$use]
      op$lc$use <- op$lc$use[op$lc$use]
    }else{
      op$lc <- NULL
    }
  }
  
  if(!is.null(op$qc)){
    Seq <- rev(seq_along(op$qc))
    for(i in Seq){
      if(!op$qc[[i]]$use){
        op$qc[[i]] <- NULL
      }
    }
    if(length(op$qc)==0){
      op$qc <- NULL
      }
  }
  
  if(!is.null(op$rc)){
    Seq <- rev(seq_along(op$rc))
    for(i in Seq){
      if(!op$rc[[i]]$use){
        op$rc[[i]] <- NULL
      }
    }
    if(length(op$rc)==0){
      op$rc <- NULL
    }
  }
  
  op
}