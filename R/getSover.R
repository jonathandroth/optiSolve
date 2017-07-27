
"getSolver"<-function(op, quiet=FALSE){
  if(class(op$f)=="quadFun"){ solver <- "cccp"}
  if(class(op$f)=="ratioFun"){solver <- "slsqp"}
  if(class(op$f)=="linFun"){  solver <- "cccp2"}
  if((class(op$f)=="linFun") && is.null(op$lb) && is.null(op$ub) && !is.null(op$qc)){solver <- "alabama"}

  solver
}