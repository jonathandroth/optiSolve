\name{adjust}
\Rdversion{1.0}
\alias{adjust}
\title{Adjust Constraints and Objective Functions}

\description{
Constraints and objective functions are adjusted so that they refer to a larger or smaller set of variables.
}

\usage{
adjust(x, ids)
}

\arguments{
\item{x}{Constraint or objective function of class \code{"linFun"}, \code{"linCon"}, \code{"quadFun"}, \code{"quadCon"}, \code{"ratioFun"}, and \code{"ratioCon"}.}
\item{ids}{Vector with ids of the variables.}
}

\details{
Constraints and objective functions are adjusted so that they refer to a larger or smaller set of variables. Additional variables do not affect the value of the constraint or objective function.

}

\value{
A data frame (invisible) containing values and bounds of the constraints, the value of the objective function, and column \code{valid} which is TRUE if all constraints are fulfilled. 
}

\seealso{
The main function for solving constrained programming problems is \link{solvecop}.
}

