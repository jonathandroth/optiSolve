
##################################################################
# This function transforms an optimization problem (op) into     #
# another equivalent optimization problem.                       #
# ->  linear equality constraints defined by                     #
#     matrices are replaced by functions.                        #
##################################################################


"eqcon2fun" <- function(op){
  if(is.null(op$eqlc)){return(op)}
  
  names(op)[names(op)=="eqlc"] <- "eqfun"
  
  
  op$eqfun$f0  = function(x){}
  op$eqfun$g0  = function(x){}
  
  body(op$eqfun$f0) <- parse(text="c(op$eqfun$A%*%x-(op$eqfun$val))")
  body(op$eqfun$g0) <- parse(text="op$eqfun$A")
  
  class(op$eqfun) <- "EqFun"
  
  op
}