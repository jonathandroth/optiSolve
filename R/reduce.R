
##################################################################
# This function transforms an optimization problem (op) into     #
# an equivalent other optimization problem.                      #
# -> Variables are removed from the model whose values are       #
#    unequivocally determined.                                   #
##################################################################


"reduce" <- function(op, rid){
  rid <- op$id[op$id %in% rid]
  id  <- op$id[!(op$id %in% rid)]
  rx  <- op$lb$val[rid]
  op$x <- setNames(rep(NA, length(op$id)), op$id)
  op$x[rid] <- op$lb$val[rid]
  
  #Remove variables from f
  if(class(op$f)=="linFun"){
    op$f$d  <- op$f$d + sum(op$f$a[rid]*rx)
    op$f$a  <- op$f$a[id]
    op$f$id <- id
  }
  
  if(class(op$f)=="quadFun"){
    op$f$d  <- op$f$d + sum(op$f$a[rid]*rx) + c(t(rx)%*%(op$f$Q[rid,rid])%*%rx)
    op$f$a  <- op$f$a[id] + 2*c(op$f$Q[id, rid]%*%rx)
    op$f$Q  <- op$f$Q[id, id]
    op$f$id <- id
  }
  
  if(class(op$f)=="ratioFun"){
    op$f$d1 <- op$f$d1 + sum(op$f$a1[rid]*rx) + c(t(rx)%*%(op$f$Q1[rid,rid])%*%rx)
    op$f$a1 <- op$f$a1[id] + 2*c(op$f$Q1[id, rid]%*%rx)
    op$f$Q1 <- op$f$Q1[id, id]
    op$f$d2 <- op$f$d2 + sum(op$f$a2[rid]*rx) + c(t(rx)%*%(op$f$Q2[rid,rid])%*%rx)
    op$f$a2 <- op$f$a2[id] + 2*c(op$f$Q2[id, rid]%*%rx)
    op$f$Q2 <- op$f$Q2[id, id]
    op$f$id <- id
  }
  
  #Remove variables from lb
  op$lb <- lbcon(val=op$lb$val[op$lb$id %in% id],id=op$lb$id[op$lb$id %in% id])
  
  #Remove variables from ub
  op$ub <- ubcon(val=op$ub$val[op$ub$id %in% id],id=op$ub$id[op$ub$id %in% id])
  
  #Remove variables from lc
  if(!is.null(op$lc)){
    op$lc$d   <- op$lc$d + c(op$lc$A[,rid]%*%rx)
    op$lc$A   <- op$lc$A[,id]
    op$lc$id  <- id
    keepCon   <- !apply(op$lc$A==0,1,all)
    Seq       <- rev(1:nrow(op$lc$A))
    for(i in Seq){
      keepRed    <- keepCon
      keepRed[i] <- FALSE
      if(any(keepRed)){
        x    <- t(ginv(op$lc$A[keepRed,,drop=FALSE]))%*%op$lc$A[i,]
        diff <- max(abs(op$lc$A[i,]-t(op$lc$A[keepRed,,drop=FALSE])%*%x))
        if(diff<0.00001){keepCon <- keepRed}
      }
    }
    op$lc$A   <- op$lc$A[keepCon, , drop=FALSE]
    op$lc$val <- op$lc$val[keepCon]
    op$lc$dir <- op$lc$dir[keepCon]
    op$lc$d   <- op$lc$d[keepCon]
    op$lc$use <- op$lc$use[keepCon]
  }
  
  #Remove variables from qc
  Seq <- seq_along(op$qc)
  for(i in Seq){
    op$qc[[i]]$d  <- op$qc[[i]]$d + sum(op$qc[[i]]$a[rid]*rx) + c(t(rx)%*%(op$qc[[i]]$Q[rid,rid])%*%rx)
    op$qc[[i]]$a  <- op$qc[[i]]$a[id] + 2*c(op$qc[[i]]$Q[id, rid]%*%rx)
    op$qc[[i]]$Q  <- op$qc[[i]]$Q[id, id]
    op$qc[[i]]$id <- id
  }
  
  #Remove variables from rc
  Seq <- seq_along(op$rc)
  for(i in Seq){
    op$rc[[i]]$d1 <- op$rc[[i]]$d1 + sum(op$rc[[i]]$a1[rid]*rx) + c(t(rx)%*%(op$rc[[i]]$Q1[rid,rid])%*%rx)
    op$rc[[i]]$a1 <- op$rc[[i]]$a1[id] + 2*c(op$rc[[i]]$Q1[id, rid]%*%rx)
    op$rc[[i]]$Q1 <- op$rc[[i]]$Q1[id, id]
    op$rc[[i]]$d2 <- op$rc[[i]]$d2 + sum(op$rc[[i]]$a2[rid]*rx) + c(t(rx)%*%(op$rc[[i]]$Q2[rid,rid])%*%rx)
    op$rc[[i]]$a2 <- op$rc[[i]]$a2[id] + 2*c(op$rc[[i]]$Q2[id, rid]%*%rx)
    op$rc[[i]]$Q2 <- op$rc[[i]]$Q2[id, id]
    op$rc[[i]]$id <- id
  }
  
  #Remove variables from id
  op$id <- id
  
  op
}