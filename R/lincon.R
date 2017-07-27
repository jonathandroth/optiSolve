
"lincon"<-function(A, d=rep(0, nrow(A)), dir=rep("==",nrow(A)), val=rep(0, nrow(A)), id=1:ncol(A), use=rep(TRUE,nrow(A)), name=rownames(A)){
  if(!is.matrix(A)){A <- as(A, "matrix")}

  d   <- c(d)
  val <- c(val)
  dir <- c(dir)
  id  <- c(id)
  use <- c(use)
  
  storage.mode(A)   <- "double"
  storage.mode(d)   <- "double"
  storage.mode(val) <- "double"
  storage.mode(dir) <- "character"
  storage.mode(id)  <- "character"
  storage.mode(name)<- "character"
  storage.mode(use) <- "logical"
  
  if(!is.vector(d)){       stop("Argument 'd' must be a numeric vector.\n")}
  if(!is.vector(val)){     stop("Argument 'val' must be a numeric vector.\n")}
  if(!is.vector(dir)){     stop("Argument 'dir' must be a character vector.\n")}
  if(!is.vector(id)){      stop("Argument 'id' (if present) must be a character vector.\n")}
  if(!is.vector(use)){     stop("Argument 'use' (if present) must be a logical vector.\n")}
  if(!is.vector(name)){    stop("Argument 'name' (if present) must be a character vector.\n")}
  if(nrow(A)!=length(d)){  stop("Length of 'd' must be equal to nrow(A).\n")}
  if(nrow(A)!=length(dir)){stop("Length of 'dir' must be equal to nrow(A).\n")}
  if(nrow(A)!=length(val)){stop("Length of 'val' must be equal to nrow(A).\n")}
  if(nrow(A)!=length(name)){stop("Length of 'name' must be equal to nrow(A).\n")}
  if(ncol(A)!=length(id)){ stop("Length of 'id' (if present) must be equal to ncol(A).\n")}
  if(nrow(A)!=length(use)){stop("Length of 'use' (if present) must be equal to nrow(A).\n")}
  if(any(is.na(A))){      stop("Matrix 'A' contains NA values.\n")}
  if(any(is.na(d))){      stop("Vector 'd' contains NA values.\n")}
  if(any(is.na(val))){    stop("Vector 'val' contains NA values.\n")}
  if(any(is.na(id))){     stop("Vector 'id' contains NA values.\n")}
  if(any(is.na(use))){    stop("Vector 'use' contains NA values.\n")}
  if(any(duplicated(id))){stop("Vector 'id' contains duplicated values.\n")}
  if(any(duplicated(name))){stop("Vector 'name' contains duplicated values.\n")}
  
  if(!all(dir %in% c("<=","==",">="))){
    stop("Argument dir must contain only '<=', '==', and '>='.\n")
  }
  
  rownames(A) <- name
  colnames(A) <- id
  
  ###   Return the constraint   ###
  
  con <- list(A=A, d=d, dir=dir, val=val, id=id, use=use, name=name)
  class(con) <- "linCon"
  con
}