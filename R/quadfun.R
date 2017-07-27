
"quadfun"<-function(Q, a=rep(0, nrow(Q)), d=0, id=1:nrow(Q), name="quad.fun"){
  if(!is.matrix(Q)){Q <- as(Q, "matrix")}

  a    <- c(a)
  d    <- c(d)
  id   <- c(id)
  name <- c(name)
  
  storage.mode(Q)   <- "double"
  storage.mode(a)   <- "double"
  storage.mode(d)   <- "double"
  storage.mode(id)  <- "character"
  storage.mode(name)<- "character"
  
  if(!is.vector(a)){      stop("Argument 'a' must be a numeric vector.\n")}
  if(!is.vector(d)){      stop("Argument 'd' must be a number.\n")}
  if(!is.vector(id)){     stop("Argument 'id' (if present) must be a character vector.\n")}
  if(!is.vector(name)){   stop("Argument 'name' (if present) must be a character string.\n")}
  if(nrow(Q)!=ncol(Q)){   stop("Matrix 'Q' must be quadratic.\n")}
  if(nrow(Q)!=length(a)){ stop("Length of vector 'a' must be equal to nrow(Q).\n")}
  if(length(d)>1){        stop("Argument 'd' must be a number.\n")}
  if(length(name)>1){     stop("Argument 'name' must be a character string.\n")}
  if(ncol(Q)!=length(id)){stop("Length of 'id' (if present) must be equal to ncol(Q).\n")}
  if(any(is.na(d))){      stop("Parameter 'd' is NA.\n")}
  if(any(is.na(a))){      stop("Vector 'a' contains NA values.\n")}
  if(any(is.na(Q))){      stop("Matrix 'Q' contains NA values.\n")}
  if(any(is.na(id))){     stop("Vector 'id' contains NA values.\n")}
  if(any(is.na(name))){   stop("Argument 'name' is NA.\n")}
  if(any(duplicated(id))){stop("Vector 'id' contains duplicated values.\n")}
  
  rownames(Q) <- id
  colnames(Q) <- id
  names(a)    <- id
  
  Range <- range(Q-t(Q))
  if(Range[2]-Range[1]>1e-09){
    Q <- (Q + t(Q))/2
  }
  
  ###   Return the function as a list   ###
  fun <- list(Q=Q, a=a, d=d, id=id, name=name)
  class(fun) <- "quadFun"
  fun
}