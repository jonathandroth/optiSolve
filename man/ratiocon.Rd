\name{ratiocon}
\Rdversion{1.0}
\alias{ratiocon}
\title{Rational Constraint}

\description{
Define a rational constraint of the form \deqn{\frac{x^T Q_1 x + a_1^T x + d_1}{x^T Q_2 x + a_2^T x + d_2} \leq val}{(x' Q1 x + a1' x + d1)/(x' Q2 x + a2' x + d2) <= val}
}
\usage{
ratiocon(Q1, a1=rep(0, nrow(Q1)), d1=0, Q2, a2=rep(0, nrow(Q2)), d2=0, dir="<=", val, 
   id=1:nrow(Q1), name="rational", use=TRUE)
}

\arguments{
\item{Q1}{Numeric quadratic matrix.}
\item{a1}{Numeric vector.}
\item{d1}{Numeric value.}
\item{Q2}{Numeric quadratic matrix.}
\item{a2}{Numeric vector.}
\item{d2}{Numeric value.}
\item{dir}{Character string \code{"<="}.}
\item{val}{Numeric threshold value, which is the upper bound for the rational function.}
\item{id}{Vector defining the names of the variables to which the constraint applies. Each variable name corresponds to one component of \code{x}. Variable names must be consistent across constraints.}
\item{name}{Name for the constraint.}
\item{use}{Logical value indicating if the constraint should be included in the optimization problem. If \code{use=FALSE}, then the constraint  does not affect the result, but the value of the rational function will be reported by function \link{validate}.}
}

\details{
Define a rational inequality constraint of the form \deqn{\frac{x^T Q_1 x + a_1^T x + d_1}{x^T Q_2 x + a_2^T x + d_2} \leq val.}{(x' Q1 x + a1' x + d1)/(x' Q2 x + a2' x + d2) <= val.}
Vector \code{x} contains only the variables included in argument \code{id}.

For rational constraints it is required that there is a linear constraint ensuring that \code{sum(x)} is a constant. Furthermore, the denominator must be non-negative.

}

\value{
An object of class \code{ratioCon}.
}

\seealso{
The main function for solving constrained programming problems is \code{\link{solvecop}}.
}

\examples{

### Constrained optimization with rational objective                  ### 
### function and linear and quadratic constraints                     ###
### Example from animal breeding                                      ###
### The mean kinship at native alleles in the offspring is minimized  ###
### The mean breeding value and the mean kinship are constrained      ###

data(phenotype)
data(myQ)
data(myQ1)
data(myQ2)

A <- t(model.matrix(~Sex+BV+MC-1, data=phenotype))
A[,1:5]
val <- c(0.5,  0.5,  0.4,  0.5 )
dir <- c("==", "==", ">=", "<=")

mycop <- cop(f  = quadfun(Q=myQ, d=0.001, name="Kinship", id=rownames(myQ)),
             lb = lbcon(0,  id=phenotype$Indiv), 
             ub = ubcon(NA, id=phenotype$Indiv),
             lc = lincon(A=A, dir=dir, val=val, id=phenotype$Indiv),
             rc = ratiocon(Q1=myQ1, Q2=myQ2, d1=0.0004, d2=0.00025, val=0.040, 
                           id=rownames(myQ1), name="nativeKinship")
             )

res <- solvecop(mycop, solver="slsqp", quiet=FALSE)

validate(mycop, res)

#            valid solver                status
#             TRUE  slsqp successful completion
#
#   Variable      Value      Bound    OK?
#   --------------------------------------
#   Kinship       0.0324 min        :      
#   --------------------------------------
#   lower bounds  all x  >=  lb     : TRUE 
#   Sexfemale     0.5    ==  0.5    : TRUE 
#   Sexmale       0.5    ==  0.5    : TRUE 
#   BV            0.4    >=  0.4    : TRUE 
#   MC            0.4668 <=  0.5    : TRUE 
#   nativeKinship 0.04   <=  0.04   : TRUE 
#   --------------------------------------

}




