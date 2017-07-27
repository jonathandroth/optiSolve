\name{quadcon}
\Rdversion{1.0}
\alias{quadcon}
\title{Quadratic Constraint}

\description{
Define a quadratic constraint of the form 
\deqn{x'Qx + a' x + d \leq val}{x' Q x + a' x + d <= val}
}

\usage{
quadcon(Q, a=rep(0, nrow(Q)), d=0, dir="<=", val, 
   id=1:nrow(Q), name="quadratic", use=TRUE)
}

\arguments{
\item{Q}{Numeric symmetric matrix of the constraint coefficients.}
\item{a}{Numeric vector.}
\item{d}{Numeric value.}
\item{dir}{Character string \code{"<="}.}
\item{val}{Numeric threshold value, which is the upper bound for the quadratic function.}
\item{id}{Vector defining the names of the variables to which the constraint applies. Each variable name corresponds to one component of \code{x}. Variable names must be consistent across constraints. }
\item{name}{Name for the constraint.}
\item{use}{Logical value indicating if the constraint should be included in the optimization problem. If \code{use=FALSE}, then  constraint  does not affect the result, but the value of the quadratic function will be reported by function \link{validate}.}
}

\details{
Define a quadratic inequality constraint of the form 
\deqn{x'Qx + a' x + d \leq val.}{x' Q x + a' x + d <= val.}
Vector \code{x} contains only the variables included in argument \code{id}.
}

\value{
An object of class \code{quadCon}.
}

\seealso{
The main function for solving constrained programming problems is \code{\link{solvecop}}.
}

\examples{

### Linear programming with linear and quadratic constraints ###
### Example from animal breeding                             ###
### The mean breeding value BV is maximized whereas the      ###
### mean kinship in the offspring x'Qx+d is restricted       ###

data(phenotype)
data(myQ)

A   <- t(model.matrix(~Sex-1, data=phenotype))
A[,1:5]
val <- c(0.5, 0.5)
dir <- c("==","==")

mycop <- cop(f  = linfun(a=phenotype$BV, id=phenotype$Indiv, name="BV"),
             max= TRUE,
             lb = lbcon(0,  id=phenotype$Indiv), 
             ub = ubcon(NA, id=phenotype$Indiv),
             lc = lincon(A=A, dir=dir, val=val, id=phenotype$Indiv),
             qc = quadcon(Q=myQ, d=0.001, val=0.035, name="Kinship", id=rownames(myQ)))

res <- solvecop(mycop, solver="cccp2", quiet=FALSE)

validate(mycop, res)

#            valid solver  status
#             TRUE  cccp2 optimal
#
#   Variable     Value      Bound    OK?
#   -------------------------------------
#   BV           0.7667 max        :      
#   -------------------------------------
#   lower bounds all x  >=  lb     : TRUE 
#   Sexfemale    0.5    ==  0.5    : TRUE 
#   Sexmale      0.5    ==  0.5    : TRUE 
#   Kinship      0.035  <=  0.035  : TRUE 
#   -------------------------------------

}


