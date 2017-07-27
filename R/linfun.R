
"linfun"<-function(a, d=0, id=1:length(a), name="lin.fun"){

  a    <- c(a)
  d    <- c(d)
  id   <- c(id)
  name <- c(name)
  
  storage.mode(a)    <- "double"
  storage.mode(d)    <- "double"
  storage.mode(id)   <- "character"
  storage.mode(name) <- "character"
  
  if(!is.vector(a)){        stop("Argument 'a' must be a numeric vector.\n")}
  if(!is.vector(d)){        stop("Argument 'd' must be a number.\n")}
  if(!is.vector(name)){     stop("Argument 'name' must be a character string\n")}
  if(!is.vector(id)){       stop("Argument 'id' (if present) must be a character vector.\n")}
  if(length(d)>1){          stop("Argument 'd' must be a number.\n")}
  if(length(name)>1){       stop("Argument 'name' must be a character string.\n")}
  if(length(a)!=length(id)){stop("Length of 'id' (if present) must be equal to length(a).\n")}
  if(is.na(d)){             stop("Value 'd' is NA.\n")}
  if(any(is.na(a))){        stop("Vector 'a' contains NA values.\n")}
  if(any(is.na(id))){       stop("Vector 'id' contains NA values.\n")}
  if(any(duplicated(id))){  stop("Vector 'id' contains duplicated values.\n")}
  
  names(a) <- id

  ###   Return the function as a list   ### 
  fun <- list(a=a, d=d, id=id, name=name)
  class(fun) <- "linFun"
  fun
}