\name{quadfun}
\Rdversion{1.0}
\alias{quadfun}
\title{Quadratic Objective Function}

\description{
Define a quadratic objective function of the form \deqn{f(x) = x^T Qx + a^T x + d}{f(x) = x' Q x + a' x + d}
}
\usage{
quadfun(Q, a=rep(0, nrow(Q)), d=0, id=1:nrow(Q), name="quad.fun")
}

\arguments{
\item{Q}{Numeric symmetric matrix of the constraint coefficients.}
\item{a}{Numeric vector.}
\item{d}{Numeric value.}
\item{id}{Vector (if present), defining the names of the variables to which the function applies. Each variable name corresponds to one component of \code{x}. Variable names must be consistent across constraints. }
\item{name}{Name for the objective function.}
}

\details{
Define a quadratic objective function of the form \deqn{f(x) = x^T Qx + a^T x + d}{f(x) = x' Q x + a' x + d}
}

\value{
An object of class \code{quadFun}.
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


