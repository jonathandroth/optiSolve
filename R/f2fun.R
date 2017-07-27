
##############################################################
# This function transforms an optimization problem (op) into #
# an equivalent other optimization problem                   #
# -> An objective function of class ratioFun is replaced by  #
#    an objective function of class Fun.                     #
##############################################################

"f2fun" <- function(op){
 
  op$f$f0 <- function(x){0}
  op$f$g0 <- function(x){rep(0, length(x))}
  op$f$h0 <- function(x){matrix(0, nrow(x),ncol(x))}
  
  
  if(class(op$f)=="linFun"){
    op$f$Zeros <- matrix(0, length(op$f$a), length(op$f$a))
    op$f$f0 <- function(x){sum((op$f$a)*x)}
    op$f$g0 <- function(x){op$f$a}
    op$f$h0 <- function(x){op$f$Zeros}
  }
  
  if(class(op$f)=="quadFun"){
    op$f$f0 <- function(x){c(t(x)%*%(op$f$Q)%*%x + t(op$f$a)%*%x + (op$f$d))}
    op$f$g0 <- function(x){c(2*(op$f$Q)%*%x + (op$f$a))}
    op$f$h0 <- function(x){2*(op$f$Q)}
  }
  
  if(class(op$f)=="ratioFun"){
    op$f$f0 <- function(x){
      c(t(x)%*%(op$f$Q1)%*%x+t((op$f$a1))%*%x+(op$f$d1))/c(t(x)%*%(op$f$Q2)%*%x+t((op$f$a2))%*%x+(op$f$d2))
    }
    
    op$f$g0 <- function(x){
      gx <- c(t(x)%*%(op$f$Q1)%*%x+t((op$f$a1))%*%x+(op$f$d1))
      hx <- c(t(x)%*%(op$f$Q2)%*%x+t((op$f$a2))%*%x+(op$f$d2))
      
      (1/hx)*c(2*(op$f$Q1)%*%x+(op$f$a1)) - (gx/(hx^2))*c(2*(op$f$Q2)%*%x+(op$f$a2))
    }
    
    op$f$h0 <- function(x){
      gx  <- c(t(x)%*%(op$f$Q1)%*%x+t((op$f$a1))%*%x+(op$f$d1))
      hx  <- c(t(x)%*%(op$f$Q2)%*%x+t((op$f$a2))%*%x+(op$f$d2))
      ax  <- 1/hx
      bx  <- - gx/(hx^2)
      dax <- - (1/hx^2)*(2*(op$f$Q2)%*%x+(op$f$a2))
      dbx <- - (1/hx^2)*(2*(op$f$Q1)%*%x+(op$f$a1)) + (2*gx/hx^3)*(2*(op$f$Q2)%*%x+(op$f$a2))
      ax*(2*(op$f$Q1)) + dax%*%t(2*(op$f$Q1)%*%x+(op$f$a1))  +  bx*(2*(op$f$Q2)) + dbx%*%t(2*(op$f$Q2)%*%x+(op$f$a2))
    }
  }
  
  class(op$f) <- "Fun"
  op
}


