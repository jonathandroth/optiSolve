
"ratiocon"<-function(Q1, a1=rep(0, nrow(Q1)), d1=0, Q2, a2=rep(0, nrow(Q2)), d2=0, dir="<=", val, id=1:nrow(Q1), name="rational", use=TRUE){
  if(!identical(dir,"<=")){stop("Only dir='<=' is implemented.\n")}
  
  ###   Check if the constraint is well defined   ###
  
  if(!is.matrix(Q1)){Q1 <- as(Q1, "matrix")}
  if(!is.matrix(Q2)){Q2 <- as(Q2, "matrix")}

  a1   <- c(a1)
  a2   <- c(a2)
  d1   <- c(d1)
  d2   <- c(d2)
  val  <- c(val)
  id   <- c(id)
  use  <- c(use)
  name <- c(name)
  
  storage.mode(Q1)  <- "double"
  storage.mode(Q2)  <- "double"
  storage.mode(a1)  <- "double"
  storage.mode(a2)  <- "double"
  storage.mode(d1)  <- "double"
  storage.mode(d2)  <- "double"
  storage.mode(val) <- "double"
  storage.mode(id)  <- "character"
  storage.mode(name)<- "character"
  storage.mode(use) <- "logical"
  
  if(!is.vector(a1)){      stop("Argument 'a1' must be a numeric vector.\n")}
  if(!is.vector(a2)){      stop("Argument 'a2' must be a numeric vector.\n")}
  if(!is.vector(d1)){      stop("Argument 'd1' must be a number.\n")}
  if(!is.vector(d2)){      stop("Argument 'd2' must be a number.\n")}
  if(!is.vector(val)){     stop("Argument 'val' must be a number.\n")}
  if(!is.vector(use)){     stop("Argument 'use' must be TRUE or FALSE.\n")}
  if(!is.vector(id)){      stop("Argument 'id' (if present) must be a character vector.\n")}
  if(!is.vector(name)){    stop("Argument 'name' must be a character string.\n")}
  if(nrow(Q1)!=ncol(Q1)){  stop("Matrix 'Q1' must be quadratic.\n")}
  if(nrow(Q2)!=ncol(Q2)){  stop("Matrix 'Q2' must be quadratic.\n")}
  if(nrow(Q1)!=nrow(Q2)){  stop("Matrices 'Q1' and 'Q2' must have the same dimension.\n")}
  if(nrow(Q1)!=length(a1)){stop("Length of vector 'a1' must be equal to nrow(Q1).\n")}
  if(nrow(Q2)!=length(a2)){stop("Length of vector 'a2' must be equal to nrow(Q2).\n")}
  if(length(d1)>1){        stop("Argument 'd1' must be a number.\n")}
  if(length(d2)>1){        stop("Argument 'd2' must be a number.\n")}
  if(length(val)>1){       stop("Argument 'val' must be a number.\n")}
  if(length(use)>1){       stop("Argument 'use' must be TRUE or FALSE.\n")}
  if(length(name)>1){      stop("Argument 'name' must be a character string.\n")}
  if(ncol(Q1)!=length(id)){stop("Length of 'id' (if present) must be equal to ncol(Q1).\n")}
  if(any(is.na(val))){     stop("Parameter 'val' is NA.\n")}
  if(any(is.na(use))){     stop("Parameter 'use' is NA.\n")}
  if(any(is.na(d1))){      stop("Parameter 'd1' is NA.\n")}
  if(any(is.na(a1))){      stop("Vector 'a1' contains NA values.\n")}
  if(any(is.na(Q1))){      stop("Matrix 'Q1' contains NA values.\n")}
  if(any(is.na(d2))){      stop("Parameter 'd2' is NA.\n")}
  if(any(is.na(a2))){      stop("Vector 'a2' contains NA values.\n")}
  if(any(is.na(Q2))){      stop("Matrix 'Q2' contains NA values.\n")}
  if(any(is.na(id))){      stop("Vector 'id' contains NA values.\n")}
  if(is.na(name)){         stop("Argument 'name' is NA.\n")}
  if(any(duplicated(id))){ stop("Vector 'id' contains duplicated values.\n")}
  
  rownames(Q1) <- id
  colnames(Q1) <- id
  rownames(Q2) <- id
  colnames(Q2) <- id
  names(a1)    <- id
  names(a2)    <- id
  
  Range <- range(Q1-t(Q1))
  if(Range[2]-Range[1]>1e-09){
    Q1 <- (Q1 + t(Q1))/2
  }
  
  Range <- range(Q2-t(Q2))
  if(Range[2]-Range[1]>1e-09){
    Q2 <- (Q2 + t(Q2))/2
  }
  
  ###   Return the constraint ###
  con <- list(Q1=Q1, a1=a1, d1=d1, Q2=Q2, a2=a2, d2=d2, dir=dir, val=val, id=id, name=name, use=use)
  class(con) <- "ratioCon"
  con
}