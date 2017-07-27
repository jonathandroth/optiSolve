\name{lincon}
\Rdversion{1.0}
\alias{lincon}
\title{Linear Constraints}

\description{
Define linear equality and inequality constraints of the form \deqn{A x + d dir val}{A x + d dir val} 
}
\usage{
lincon(A, d=rep(0, nrow(A)), dir=rep("==",nrow(A)), val=rep(0, nrow(A)), 
   id=1:ncol(A), use=rep(TRUE,nrow(A)), name=rownames(A))
}

\arguments{
\item{A}{Numeric matrix of the constraint coefficients.}
\item{d}{Numeric vector.}
\item{dir}{Character vector with the directions of the constraints. Each element must be one of  \code{"<="}, \code{"=="}, and \code{">="}.}
\item{val}{Numeric vector with threshold values.}
\item{id}{Vector (if present), defining the names of the variables to which the constraint applies. Each variable name corresponds to one component of \code{x}. Variable names must be consistent across constraints. 
}
\item{use}{Logical vector indicating the constraints to be included in the optimization problem. If \code{use[i]=FALSE}, then linear constraint \code{i} does not affect the result, but the value of the linear function \code{A[i,] x + d[i]} will be reported by function \link{validate}.}
\item{name}{Vector with names of the constraints.}
}

\details{
Define linear  inequality and equality constraints of the form 
\deqn{A x + d dir val}{A x + d dir val} 
(component wise). If parameter \code{id} is specified, then vector \code{x} contains only the indicated variables.



}

\value{
An object of class \code{linCon}.
}

\seealso{
The main function for solving constrained programming problems is \code{\link{solvecop}}.
}

\examples{

### Quadratic programming with linear constraints       ###
### Example from animal breeding                        ###
### The mean kinship in the offspring x'Qx+d is minized ###
### and the mean breeding value is restricted.          ###

data(phenotype)
data(myQ)

A   <- t(model.matrix(~Sex+BV-1, data=phenotype))
A[,1:5]
val <- c(0.5, 0.5, 0.40)
dir <- c("==","==",">=")

mycop <- cop(f  = quadfun(Q=myQ, d=0.001, name="Kinship", id=rownames(myQ)), 
             lb = lbcon(0,  id=phenotype$Indiv), 
             ub = ubcon(NA, id=phenotype$Indiv),
             lc = lincon(A=A, dir=dir, val=val, id=phenotype$Indiv))

res <- solvecop(mycop, solver="cccp", quiet=FALSE)

validate(mycop, res)

#            valid solver  status
#             TRUE   cccp optimal
#
#   Variable     Value      Bound    OK?
#   -------------------------------------
#   Kinship      0.0322 min        :      
#   -------------------------------------
#   lower bounds all x  >=  lb     : TRUE 
#   Sexfemale    0.5    ==  0.5    : TRUE 
#   Sexmale      0.5    ==  0.5    : TRUE 
#   BV           0.4    >=  0.4    : TRUE 
#   -------------------------------------


}



