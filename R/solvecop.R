

"solvecop"<-function(op, solver="default", make.definite=FALSE, X=NULL, quiet=FALSE, ...){
  if(class(op)!="coProblem"){stop("Argument op has not class 'coProblem'.")}
  if(!solver %in% c("alabama","cccp","cccp2","csdp","slsqp","default")){stop("Unknown solver.\n")}

  opt <- list(...)

  ### Remove unused constraints #########
  op <- clearup(op)

  if(solver=="default"){
    solver <- getSolver(op, quiet=quiet)
  }  
    
  #### Remove fixed variables ###########
  if(!is.null(op$lb) && !is.null(op$ub)){
    rid <- intersect(op$lb$id, op$ub$id)
    rid <- rid[op$lb$val[rid]==op$ub$val[rid]]
    if(length(rid)>0){
      op <- reduce(op, rid)
    }
  }
  

  
  #### Make qc from rc ##################
  if(length(op$rc)>0){
    op <- rc2qc(op, quiet=quiet)
  }

  #### Simplify qc and lc  ##############
  op <- simplify(op)
  
  #Now lc:        Ax dir val, with dir='==' or dir='<='
  #    qc: x'Qx+ a'x <=  val
   
  #### Make matrices definite ###########
  if(make.definite){
    op <- asdefinite(op, quiet=quiet)
  }else{
    op$madeDefinite <- FALSE
  }
  
  #### Check Start values ###########
  if(!is.null(X)){
    X <- c(X)
    storage.mode(X) <- "double"
    if(!is.vector(X)){          stop("Argument X with starting values (if provided) must be a numeric vector.\n")}
    if(is.null(names(X))){      stop("Vector X with starting values (if provided) must have component names.\n")}
    if(!all(names(X)%in%names(op$x))){stop("Some component names of vector X are not variable names.\n")}
    if(!all(op$id%in%names(X))){stop("Starting values are missing for some variables.\n")}
    X <- X[op$id]
  }

  #### Solve the optimization problem ###
  if(solver=="cccp2" && length(op$qc)==0){solver<-"cccp"}
  
  if(solver=="cccp"   ){res <- call_cccp(op,    X, opt, quiet=quiet)}
  if(solver=="cccp2"  ){res <- call_cccp2(op,   X, opt, quiet=quiet)}
  if(solver=="slsqp"  ){res <- call_slsqp(op,   X, opt, quiet=quiet)}
  if(solver=="alabama"){res <- call_alabama(op, X, opt, quiet=quiet)}
  if(solver=="csdp"   ){res <- call_csdp(op,    X, opt, quiet=quiet)}
  
  ### Add removed variables ###
  
  res$x <- replace(op$x, names(res$x), res$x)
  
  res
}