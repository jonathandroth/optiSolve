
##################################################################
# This function transforms an optimization problem (op) into     #
# another equivalent optimization problem.                       #
# ->  linear and quadratic inequality constraints defined by     #
#     matrices are replaced by functions.                        #
##################################################################


"incon2fun" <- function(op, multiDim=TRUE, withLinCon=TRUE, leq=TRUE){
  nlc <- withLinCon && !is.null(op$inlc)
  nqc <- length(op$qc)
  n   <- nlc+nqc
  
  if(n==0){return(op)}

  op$infun <- list(Q    = vector("list", n), 
                   a    = vector("list", n), 
                   val  = vector("list", n),
                   type = c(rep("lc", nlc), rep("qc", nqc)))
  
  Body     <- character(n)
  Body.jac <- character(n)
  Body.hes <- character(n)
  
  if(nlc>0){
    op$infun$a[[1]]   <- op$inlc$A
    op$infun$val[[1]] <- op$inlc$val
    
    Body[1]     <- "c((op$infun$a[[1]])%*%x-(op$infun$val[[1]]))"
    Body.jac[1] <- "op$infun$a[[1]]"
    Body.hes[1] <- "matrix(0, nrow=length(x), ncol=length(x))"
  }
  
  for(i in seq_along(op$qc)){
    op$infun$Q[[nlc+i]]   <- op$qc[[i]]$Q
    op$infun$a[[nlc+i]]   <- op$qc[[i]]$a
    op$infun$val[[nlc+i]] <- op$qc[[i]]$val - op$qc[[i]]$d
    Body[nlc+i]     <- paste("c(t(x)%*%(op$infun$Q[[",nlc+i,"]])%*%x+t(op$infun$a[[",nlc+i,"]])%*%x-op$infun$val[[",nlc+i,"]])", sep="")
    Body.jac[nlc+i] <- paste("c(2*t(x)%*%(op$infun$Q[[",nlc+i ,"]])+t(op$infun$a[[",nlc+i,"]]))", sep="")
    Body.hes[nlc+i] <- paste("2*(op$infun$Q[[",nlc+i ,"]])", sep="")
    }
  
  if(!leq){
    Body     <- paste0("(-1)*", Body)
    Body.jac <- paste0("(-1)*", Body.jac)
    Body.hes <- paste0("(-1)*", Body.hes)
  }
  
  if(multiDim){
    Body      <- paste(Body,     collapse=",")
    Body.jac  <- paste(Body.jac, collapse=",")
    Body      <- paste("c(",     Body,     ")",  sep="")
    Body.jac  <- paste("rbind(", Body.jac, ")",  sep="")

    op$infun$f0 <- function(x){}
    op$infun$g0 <- function(x){}
    
    body(op$infun$f0) <- parse(text=Body)
    body(op$infun$g0) <- parse(text=Body.jac)
  }else{
    op$infun$f0  <- rep(list(function(x){}), n)
    op$infun$g0  <- rep(list(function(x){}), n)
    op$infun$h0  <- rep(list(function(x){}), n)
    for(i in 1:n){
      body(op$infun$f0[[i]]) <- parse(text=Body[i]    )
      body(op$infun$g0[[i]]) <- parse(text=Body.jac[i])
      body(op$infun$h0[[i]]) <- parse(text=Body.hes[i])
    }
  }
  
  class(op$infun) <- "InFun"
  
  op$qc   <- NULL
  if(withLinCon){op$inlc <- NULL}
  
  op
}