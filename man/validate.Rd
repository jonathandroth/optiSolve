\name{validate}
\Rdversion{1.0}
\alias{validate}
\title{Validate a Solution}

\description{
Validate a solution of an optimization problem. 
}

\usage{
validate(op, sol, quiet=FALSE, tol=0.0001)
}

\arguments{
\item{op}{The constrained optimization problem defined with function \link{cop}.}
\item{sol}{The solution of the optimization problem obtained with function \link{solvecop}.}
\item{quiet}{Logical variable indicating whether output to console should be switched off.}
\item{tol}{The tolerance. A constraint is considered fulfilled even if the value exceeds (falls below) the thresshold value by \code{tol}.}
}

\details{
Validate a solution of an optimization problem by checking if the constraints are fulfilled.

Values and bounds of the constraints are printed.
}

\value{

A list of class \code{copValidation} with components:

\item{summary}{Data frame containing one row for each constraint with the value of the constraint in column \code{Val}, the bound for the constraint in column \code{Bound}, and column \code{OK} states if the constraint is fulfilled.  The value of the objective function is shown in the first row.  Additional rows contain the values of disabled constraints.}

\item{info}{Data frame with component \code{valid} indicating if all constraints are fulfilled, component \code{solver} containing the name of the solver used for optimization, and component \code{status} describing the solution as reported by the solver.}

\item{var}{Data frame with the values of the objective function and constraints at the optimum.}

\item{obj.fun}{Named numeric value with value and name of the objective function at the optimum.}
}

\seealso{
The main function for solving constrained programming problems is \link{solvecop}.
}





\author{Robin Wellmann}
