
"ubcon"<-function(val=numeric(0), id=seq_along(val)){

  val <- c(val)
  id  <- c(id)
  
  storage.mode(val) <- "double"
  storage.mode(id)  <- "character"
  
  if(!is.vector(val)){       stop("Argument lb must be a numeric vector.\n")}
  if(!is.vector(id)){        stop("Argument id (if present) must be a character vector.\n")}
  if(length(val)==1 & length(id)>1){val <- rep(val, length(id))}
  if(length(val)!=length(id)){stop("Length of val must be 1 or equal to length(id).\n")}
  
  names(val) <- id
  
  id  <- id[!is.na(val)]
  val <- val[!is.na(val)]
  if(length(val)==0){return(NULL)}
  
  ###   Return the constraint   ###
  con <- list(val=val, id=id)
  class(con) <- "ubCon"
  con
}