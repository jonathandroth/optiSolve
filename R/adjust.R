
##################################################################
# Constraints are adjusted so that they refer to the full        #
# set of variables.                                              #
##################################################################

"adjust"<-function(x, ids){
  id <- x$id[x$id%in%ids]
  if(class(x)=="linCon"){
    u        <- x$A
    x$A      <- matrix(0, nrow=nrow(x$A), ncol=length(ids), dimnames=list(rownames(x$A), ids))
    x$A[,id] <- u[,id]
    x$id     <- ids
  }
  
  if(class(x) %in% c("linFun", "quadFun", "quadCon")){
    u       <- x$a
    x$a     <- setNames(rep(0, length(ids)), ids)
    x$a[id] <- u[id]
    x$id    <- ids
  }
  
  if(class(x) %in% c("quadFun", "quadCon")){
    u          <- x$Q
    x$Q        <- matrix(0, nrow=length(ids), ncol=length(ids), dimnames=list(ids, ids))
    x$Q[id,id] <- u[id,id]
    x$id       <- ids
  }
  
  if(class(x) %in% c("ratioFun", "ratioCon")){
    u            <- x$a1
    x$a1         <- setNames(rep(0, length(ids)), ids)
    x$a1[id]     <- u[id]
    u            <- x$a2
    x$a2         <- setNames(rep(0, length(ids)), ids)
    x$a2[id]     <- u[id]
    u            <- x$Q1
    x$Q1         <- matrix(0, nrow=length(ids), ncol=length(ids), dimnames=list(ids, ids))
    x$Q1[id, id] <- u[id,id]
    u            <- x$Q2
    x$Q2         <- matrix(0, nrow=length(ids), ncol=length(ids), dimnames=list(ids, ids))
    x$Q2[id, id] <- u[id,id]
    x$id         <- ids
  }
  
  x
}