\name{cop}
\Rdversion{1.0}
\alias{cop}
\title{Constrained Optimization Problem}

\description{
Define a constrained optimization problem with a linear, quadratic, or rational objective function, and linear, quadratic, rational, and boundary constraints.
}
\usage{
cop(f, max=FALSE, lb=NULL, ub=NULL, lc=NULL, ...)
}

\arguments{
\item{f}{Objective function, defined with function \link{linfun}, \link{quadfun}, or \link{ratiofun}.}
\item{max}{Logical value. Should the function be maximized? This is possible only for linear objective functions.}
\item{lb}{Lower bounds for the variables, defined with function \link{lbcon}.}
\item{ub}{Upper bounds for the variables, defined with function \link{ubcon}.}
\item{lc}{Linear inequality and equality constraints, defined with function \link{lincon}.}
\item{...}{Quadratic and rational inequality constraints, defined with functions \link{quadcon} and \link{ratiocon}.}
}

\details{
Define a constrained optimization problem with a linear, quadratic, or rational objective function, and linear, quadratic, rational, and boundary constraints. The optimization problem can be solved with function \link{solvecop}.
}

\value{
An object of class \code{COP}, which may contain the following components

\item{f}{List with S3-class "linFun", "quadFun", or "ratioFun", defining the objective function}
\item{max}{Logical value. Should the objective function be maximized?}
\item{lb}{List with S3-class "lbCon", defining lower bounds.}
\item{ub}{List with S3-class "ubCon", defining upper bounds.}
\item{lc}{List with S3-class "linCon", defining linear constraints}
\item{qc}{List with S3-class "quadCon", defining quadratic constraints}
\item{rc}{List with S3-class "ratioCon", defining rational constraints}
\item{x}{Vector with NAs}
\item{id}{Vector with names of the variables that are to be optimized}
\item{madeDefinite}{Logical variable indicating whether non-positive-semidefinite matrices have already been approximated by positive-definite matrices.}
}

\seealso{
The main function for solving constrained programming problems is \link{solvecop}.
}





\author{Robin Wellmann}
