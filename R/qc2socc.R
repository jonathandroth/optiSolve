
##################################################################
# This function transforms an optimization problem (op) into     #
# an equivalent other optimization problem.                      #
# -> Quadratic constraints are replaced by second order          #
#    cone constraints.                                           #
##################################################################

"qc2socc" <- function(op, quiet=FALSE){
  nqc <- length(op$qc)
  if(nqc==0){return(op)}
  
  op$socc <- append(vector("list", nqc), op$socc)
  for(i in 1:nqc){
    if(!op$madeDefinite){op$qc[[i]]$Q <- asDefinite(op$qc[[i]]$Q, quiet=quiet)}
    F <- as(chol(op$qc[[i]]$Q),"matrix")
    g <- 0.5*c(solve(t(F))%*%(op$qc[[i]]$a))
    f <- c(sqrt(op$qc[[i]]$val + sum(g*g)))
    d <- 0*g
    op$socc[[i]] <- list(F=F, g=g, f=f, d=d, id=op$qc[[i]]$id)
    class(op$socc[[i]]) <- "socCon"
  }
  
  op$qc <- NULL
  op
}