
"validate"<-function(op, sol, quiet=FALSE, tol=0.0001){
  VarName <- deparse(substitute(sol))[1]
  if(str_detect(VarName,"\\(")||VarName=="res"){VarName <- "1"}

  x <- sol$x

  ### Evaluate objective function ###
  if(class(op$f)=="linFun"){
    val <- c(t(op$f$a)%*%x + op$f$d)
  }
  if(class(op$f)=="quadFun"){
    val <- c(t(x)%*%(op$f$Q)%*%x + t(op$f$a)%*%x + op$f$d)
  }
  if(class(op$f)=="ratioFun"){
    val <- c(t(x)%*%(op$f$Q1)%*%x + t(op$f$a1)%*%x + op$f$d1)/c(t(x)%*%(op$f$Q2)%*%x + t(op$f$a2)%*%x + op$f$d2)
  }

  Sy <- data.frame(Var=op$f$name, Val=val, Dir=ifelse(op$max,"max","min"), Bound=as.numeric(NA), OK=as.logical(NA), stringsAsFactors=FALSE)
  
  valid <- TRUE
 
  ### Check lower bounds ###
  if(!is.null(op$lb)){
    isOK  <- all(x[op$lb$id] + tol >= op$lb$val)
    isOK  <- !is.na(isOK)&isOK
    valid <- valid && isOK
    Sy <- rbind(Sy, data.frame(Var="lower bounds", Val=NA, Dir=">=", Bound=NA, OK=isOK, stringsAsFactors=FALSE))
  }
  
  ### Check upper bounds ###
  if(!is.null(op$ub)){
    isOK  <- all(x[op$ub$id] <= op$ub$val + tol)
    isOK  <- !is.na(isOK)&isOK
    valid <- valid && isOK
    Sy <- rbind(Sy, data.frame(Var="upper bounds", Val=NA, Dir="<=", Bound=NA, OK=isOK, stringsAsFactors=FALSE))
  }

  ### Check linear constraints ###
  if(!is.null(op$lc)){
    lcname <- rownames(op$lc$A)
    if(is.null(lcname)){
      lcname <- paste0("lc.", 1:nrow(op$lc$A))
    }
    for(i in 1:nrow(op$lc$A)){
      name <- lcname[i]
      val  <- c(op$lc$A[i,]%*%x+op$lc$d[i])
      dir  <- op$lc$dir[i]
      cval <- ifelse(op$lc$use[i], op$lc$val[i], NA)

      if(op$lc$use[i]){
        if(dir=="<="){isOK <- val <= cval+tol}
        if(dir=="=="){isOK <- (val+tol >= cval) && (val <= cval+tol)}
        if(dir==">="){isOK <- val+tol >= cval}
        isOK  <- !is.na(isOK)&isOK
        valid <- valid & isOK
        Sy <- rbind(Sy, data.frame(Var=name, Val=val, Dir=dir, Bound=cval, OK=isOK, stringsAsFactors=FALSE))
      }else{
        Sy <- rbind(Sy, data.frame(Var=name, Val=val, Dir="  ", Bound=NA, OK=NA, stringsAsFactors=FALSE))
      }
    }
  }
  
  ### Check quadratic constraints ###
  if(!is.null(op$qc)){
    for(i in seq_along(op$qc)){
      name <- op$qc[[i]]$name
      val  <- c(t(x)%*%(op$qc[[i]]$Q)%*%x + t(op$qc[[i]]$a)%*%x + (op$qc[[i]]$d))
      dir  <- op$qc[[i]]$dir
      cval <- ifelse(op$qc[[i]]$use, op$qc[[i]]$val, NA)
      if(op$qc[[i]]$use){
        isOK  <-  val <= cval+tol
        isOK  <- !is.na(isOK)&isOK
        valid <- valid & isOK
        Sy <- rbind(Sy, data.frame(Var=name, Val=val, Dir=dir, Bound=cval, OK=isOK, stringsAsFactors=FALSE))
      }else{
        Sy <- rbind(Sy, data.frame(Var=name, Val=val, Dir="  ", Bound=NA, OK=NA, stringsAsFactors=FALSE))
      }
    }
  }

  ### Check rational constraints ###
  if(!is.null(op$rc)){
    for(i in seq_along(op$rc)){
      name <- op$rc[[i]]$name
      val  <- c(t(x)%*%(op$rc[[i]]$Q1)%*%x + t(op$rc[[i]]$a1)%*%x + (op$rc[[i]]$d1))/c(t(x)%*%(op$rc[[i]]$Q2)%*%x + t(op$rc[[i]]$a2)%*%x + (op$rc[[i]]$d2))
      dir  <- op$rc[[i]]$dir
      cval <- ifelse(op$rc[[i]]$use, op$rc[[i]]$val, NA)
      
      if(op$rc[[i]]$use){
        isOK  <- val <= cval+tol
        isOK  <- !is.na(isOK)&isOK
        valid <- valid & isOK
        Sy <- rbind(Sy, data.frame(Var=name, Val=val, Dir=dir, Bound=cval, OK=isOK, stringsAsFactors=FALSE))
      }else{
        Sy <- rbind(Sy, data.frame(Var=name, Val=val, Dir="  ", Bound=NA, OK=NA, stringsAsFactors=FALSE))
      }
    }
  }
  
  info   <- data.frame(valid=valid, solver=sol$solver, status=sol$status, row.names=VarName, stringsAsFactors=FALSE)
  result <- list(summary=Sy, info=info)

  result$var <- setNames(Sy$Val, Sy$Var)
  result$var <- result$var[!(Sy$Var %in% c("lower bounds", "upper bounds"))]
  result$var <- as.data.frame(as.list(result$var), row.names="val")
  
  result$obj.fun <- setNames(result$summary$Val[1], result$summary$Var[1])
  
  class(result) <- "copValidation"
  if(!quiet){print(result)}
  invisible(result)
}