\name{ratiofun}
\Rdversion{1.0}
\alias{ratiofun}
\title{Rational Objective Function}

\description{
Define a rational objective function of the form \deqn{f(x) = \frac{x^T Q_1 x + a_1 x + d_1}{x^T Q_2 x + a_2 x + d_2}}{f(x)  =  (x' Q1 x + a1 x + d1)/(x' Q2 x + a2 x + d2)}
}

\usage{
ratiofun(Q1, a1=rep(0, nrow(Q1)), d1=0, Q2, a2=rep(0, nrow(Q2)), d2=0, 
   id=1:nrow(Q1), name="ratio.fun")
}

\arguments{
\item{Q1}{Numeric quadratic matrix.}
\item{a1}{Numeric vector.}
\item{d1}{Numeric value.}
\item{Q2}{Numeric quadratic matrix.}
\item{a2}{Numeric vector.}
\item{d2}{Numeric value.}
\item{id}{Vector defining the names of the variables to which the constraint applies. Each variable name corresponds to one component of \code{x}. Variable names must be consistent across constraints.}
\item{name}{Name for the constraint.}
}

\details{
Define a rational ofjective function of the form \deqn{f(x) = \frac{x^T Q_1 x + a_1 x + d_1}{x^T Q_2 x + a_2 x + d_2}}{f(x)  =  (x' Q1 x + a1 x + d1)/(x' Q2 x + a2 x + d2)}
Reasonable bounds for the variables should be provided because the function can have several local optima. Solvers \code{'slsqp'} (the default) and \code{'alabama'} are recommended.
}

\value{
An object of class \code{ratioFun}.
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

Ax <- t(model.matrix(~Sex+BV+MC-1, data=phenotype))
Ax[,1:5]
val <- c(0.5,  0.5,  0.4,  0.5 )
dir <- c("==", "==", ">=", "<=")

mycop <- cop(f  = ratiofun(Q1=myQ1, Q2=myQ2, d1=0.0004, d2=0.00025, 
                           id=rownames(myQ1), name="nativeKinship"),
             lb = lbcon(0,  id=phenotype$Indiv), 
             ub = ubcon(NA, id=phenotype$Indiv),
             lc = lincon(A=Ax, dir=dir, val=val, id=phenotype$Indiv),
             qc = quadcon(Q=myQ, d=0.001, val=0.035, 
                          name="Kinship", id=rownames(myQ)))

res <- solvecop(mycop, quiet=FALSE)

validate(mycop, res)

#            valid solver                status
#             TRUE  slsqp successful completion
#
#   Variable      Value      Bound    OK?
#   --------------------------------------
#   nativeKinship 0.0366 min        :      
#   --------------------------------------
#   lower bounds  all x  >=  lb     : TRUE 
#   Sexfemale     0.5    ==  0.5    : TRUE 
#   Sexmale       0.5    ==  0.5    : TRUE 
#   BV            0.4    >=  0.4    : TRUE 
#   MC            0.4963 <=  0.5    : TRUE 
#   Kinship       0.035  <=  0.035  : TRUE 
#   --------------------------------------

}




