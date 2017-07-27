
##################################################################
# This function transforms an optimization problem (op) into     #
# an equivalent other optimization problem.                      #
# -> Bound constraints lb and ub are appended to the             #
#    linear constraints lc.                                      #
##################################################################

"bounds2lc" <- function(op){
  D <- diag(length(op$id))

  if(!is.null(op$lb)){
    haslb <- op$id %in% op$lb$id
    op$lc <- list(A   = rbind(-D[haslb,,drop=FALSE], op$lc$A),
                  d   = c(rep(0, sum(haslb)),        op$lc$d),
                  dir = c(rep("<=", sum(haslb)),     op$lc$dir),
                  val = c(-op$lb$val,                op$lc$val),
                  id  = op$lc$id)
  }

  if(!is.null(op$ub)){
    hasub <- op$id %in% op$ub$id
    op$lc <- list(A   = rbind(D[hasub,,drop=FALSE], op$lc$A),
                  d   = c(rep(0, sum(hasub)),       op$lc$d),
                  dir = c(rep("<=", sum(hasub)),    op$lc$dir),
                  val = c(op$ub$val,                op$lc$val),
                  id  = op$lc$id)
  }
  
  op$lb <- NULL
  op$ub <- NULL
  op
}