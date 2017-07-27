
##################################################################
# This function transforms an optimization problem (op) into     #
# another equivalent optimization problem.                       #
# ->  Linear ">="-constraints are replaced by "<=" constraints.  #
# ->  Linear constraints are split into                          #
#     component eqlc containing the equality constraints, and    #
#     component inlc containing the inequality constraints.      #
##################################################################

"splitlc" <- function(op){
  if(is.null(op$lc)){return(op)}
  
  eq <- op$lc$dir=="=="
  if(any(eq)){
    op$eqlc <- list(
      A   = op$lc$A[eq,,drop=FALSE],
      d   = rep(0, sum(eq)),
      dir = rep("==", sum(eq)),
      val = op$lc$val[eq]-op$lc$d[eq],
      id  = op$lc$id
    )
    class(op$eqlc) <- "eqLinCon"
  }
  
  geq <- op$lc$dir==">="
  if(any(geq)){
    op$lc$A[geq,] <- (-1)*op$lc$A[geq,,drop=FALSE]
    op$lc$val[geq]<- (-1)*op$lc$val[geq]
    op$lc$dir[geq]<- "<="
  }
  
  leq <- op$lc$dir=="<="
  if(any(leq)){
    op$inlc <- list(
      A   = op$lc$A[leq,,drop=FALSE],
      d   = rep(0, sum(leq)),
      dir = rep("<=", sum(leq)),
      val = op$lc$val[leq]-op$lc$d[leq],
      id  = op$lc$id
    )
  class(op$inlc) <- "inLinCon"
  }
  
  op$lc <- NULL
  
  return(op)
}