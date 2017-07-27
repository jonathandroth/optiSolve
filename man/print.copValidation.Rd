\name{print.copValidation}
\Rdversion{1.0}
\alias{print.copValidation}
\alias{print}
\title{Print Validation of a Solution}

\description{
Print the validation results for the solution of an optimization problem. 
}

\usage{
\method{print}{copValidation}(x, \dots)
}

\arguments{
\item{x}{The result of function \link{validate}.}
\item{...}{Unused additional arguments.}
}

\details{
Print the validation results for the solution of an optimization problem.
}

\value{

A list of class \code{copValidation} (invisible) with components:

\item{summary}{Data frame containing one row for each constraint with the value of the constraint in column \code{Val}, the bound for the constraint in column \code{Bound}, and column \code{OK} states if the constraint is fulfilled.  The value of the objective function is shown in the first row.  Additional rows contain the values of disabled constraints.}

\item{info}{Data frame with component \code{valid} indicating if all constraints are fulfilled, component \code{solver} containing the name of the solver used for optimization, and component \code{status} describing the solution as reported by the solver.}

\item{var}{Data frame with the values of the objective function and constraints at the optimum.}

\item{obj.fun}{Named numeric value with value and name of the objective function at the optimum.}

}

\seealso{
The main function for solving constrained programming problems is \link{solvecop}.
}


\examples{

### Quadratic programming with linear constraints      ###
### Example from animal breeding                       ###
### where the mean kinship in the offspring is minized ###

data(phenotype)
data(myQ)

A   <- t(model.matrix(~Sex+BV-1, data=phenotype))
rownames(A) <- c("male.cont","female.cont", "Breeding.Value")
val <- c(0.5, 0.5, 0.40)
dir <- c("==","==",">=")

mycop <- cop(f  = quadfun(Q=myQ, d=0.001, name="Kinship", id=rownames(myQ)), 
             lb = lbcon(0,  id=phenotype$Indiv), 
             ub = ubcon(NA, id=phenotype$Indiv),
             lc = lincon(A=A, dir=dir, val=val, id=phenotype$Indiv))

res <- solvecop(mycop, solver="cccp", quiet=FALSE, trace=FALSE)

head(res$x)

Evaluation <- validate(mycop, res, quiet=TRUE)

print(Evaluation)

#            valid solver  status
#             TRUE   cccp optimal
#
#   Variable       Value      Bound    OK?
#   ---------------------------------------
#   Kinship        0.0322 min        :      
#   ---------------------------------------
#   lower bounds   all x  >=  lb     : TRUE 
#   male.cont      0.5    ==  0.5    : TRUE 
#   female.cont    0.5    ==  0.5    : TRUE 
#   Breeding.Value 0.4    >=  0.4    : TRUE 
#   ---------------------------------------

}


