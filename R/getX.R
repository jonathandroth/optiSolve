
##################################################################
# Get feasible starting values if the objective is to minimize   #
# a rational function.                                           #
# The approach is to find a solution for a related optimization  #
# problem for which no starting value is needed, i.e. to minimize#
# the numerator x'Q1x+a1'x+d1 while using only variables with    #
# large diag(Q2)                                                 #
##################################################################

"getX" <- function(op){
  if(class(op$f)=="ratioFun"){
    N <- nrow(op$f$Q2)
  
    ### Define a new optimization problem op2 with a  #############
    ### possibly smaller number of variables ######################
    if(is.null(op$lc)){
      group <- rep("1", N)
    }else{
      group <- apply(1*(op$lc$A!=0), 2, paste, collapse="-")
    }
    names(group) <- op$id
    ord          <- order(diag(op$f$Q2), decreasing=TRUE)
    group        <- group[ord]
    ids          <- split(names(group), group)
  
    nid <- pmin(lapply(ids,length), round(300/length(ids)))
    ids <- mapply(function(n, id){id[1:n]}, id = ids, n = nid, SIMPLIFY=FALSE)
    ids <- unlist(ids)
    rid <- op$id[!(op$id %in% ids)]

    fixedX <- setNames(rep(0, length(rid)), rid)
    if(!is.null(op$lb)){
      index <- rid[rid %in% op$lb$id]
      fixedX[index] <- op$lb$val[index]
    }
    if(!is.null(op$ub)){
      index <- rid[rid %in% op$ub$id]
      fixedX[index] <- op$ub$val[index]
    }
  
    op2    <- op
    val    <- c(op2$lb$val[!(op2$lb$id %in% rid)], fixedX)
    op2$lb <- lbcon(val=val,id=names(val))
    val    <- c(op2$ub$val[!(op2$ub$id %in% rid)], fixedX)    
    op2$ub <- ubcon(val=val,id=names(val))
    op2$f  <- quadfun(Q=op$f$Q1, a=op$f$a1, d=op$f$d1, id=op$f$id)
    
    ### Reduce bounds for <= constraints ##############
    if(!is.null(op2$lc)){
      leq <- op2$lc$dir=="<="
      if(any(leq)){
        op2$lc$val[leq] <- op2$lc$val[leq] - 0.001*abs(op2$lc$val[leq])
      }
    }
    
    for(i in seq_along(op2$qc)){
      op2$qc[[i]]$val <- op2$qc[[i]]$val - 0.001*abs(op2$qc[[i]]$val)
    }
    for(i in seq_along(op2$socc)){
      op2$socc[[i]]$val <- op2$socc[[i]]$val - 0.001*abs(op2$socc[[i]]$val)
    }
    
    ### Find optimum solution for optimization problem op2 ###
    
    subX <- solvecop(op2, solver="default", makeDefinite=TRUE, X=NULL, quiet=TRUE)$x
    subX <- subX[names(subX)%in% op$id]
    if(max(abs(subX))<0.0001){subX<-subX+0.0001}
    ### Use this solution as the starting value ##############
    X <- setNames(rep(0, length(op$id)), op$id)
    X[names(subX)] <- subX
    X[rid]         <- fixedX

  ### Check if equality constraints are fulfilled ##########
    eq <- op$lc$dir=="=="
    if(any(eq)){
      newval   <- op$lc$A[eq,,drop=FALSE] %*% X
      distance <- mean(abs(op$lc$val[eq]-newval))
      if(distance>0.0005){stop("No feasible starting value found.\n")}
    }
  
    return(X)
  }else{
    
    if(is.null(op$lb)){
      minX <- setNames(rep(0, length(op$id)), op$id)
    }else{
      minX <- setNames(rep(mean(op$lb$val), length(op$id)), op$id)
      minX[op$lb$id] <- op$lb$val
     }

    if(is.null(op$ub)){
      maxX <- setNames(rep(2/length(op$id), length(op$id)), op$id)
    }else{
      maxX <- setNames(rep(mean(op$ub$val), length(op$id)), op$id)
      maxX[op$ub$id] <- op$ub$val
    }
    
    X <- setNames(runif(length(op$id), min=minX, max=maxX), op$id)
    return(X)
  }
}