\name{optiSolve-package}
\alias{optiSolve-package}
\alias{optiSolve}
\docType{package}
\title{
\packageTitle{optiSolve}
}
\description{
\packageDescription{optiSolve}
}
\details{


The following steps are included in solving a constrained optimization problem (cop):

\bold{1)} Define the objective with one of the following functions:

\tabular{ll}{
\link{linfun}    \tab  defines a linear objective function,\cr 
\link{quadfun}   \tab  defines a quadratic objective function,\cr
\link{ratiofun}  \tab  defines a rational objective function.\cr 
}

\bold{2)} Define the constraints by using the following functions:


\tabular{ll}{
\link{lincon}    \tab  defines linear equality and inequality constraints,\cr 
\link{quadcon}   \tab  defines quadratic constraints,\cr
\link{ratiocon}  \tab  defines rational constraints,\cr 
\link{lbcon}    \tab  defines lower bounds for the variables,\cr
\link{ubcon}    \tab  defines upper bounds for the variables.\cr
}

\bold{3)} Put the objective function and the constraints together to define the optimization problem:

\tabular{ll}{
\link{cop}    \tab  defines a constrained optimization problem.\cr 
}

\bold{4)} Solve the optimization problem:

\tabular{ll}{
\link{solvecop}    \tab  solves a constrained optimization problem.\cr 
}


\bold{5)} Check if the solution fulfils all constraints:

\tabular{ll}{
\link{validate}    \tab  checks if the solution fulfils all constraints, and calculates the values of the constraints.\cr 
}

}
\author{
\packageAuthor{optiSolve}

Maintainer: \packageMaintainer{optiSolve}
}
\references{
Borchers, B. (1999). CSDP, A C Library for Semidefinite Programming Optimization Methods and Software 11(1):613-623
\code{http://euler.nmt.edu/~brian/csdppaper.pdf}

Kraft, D. (1988). A software package for sequential quadratic programming, Technical Report DFVLR-FB 88-28, Institut fuer Dynamik der Flugsysteme, Oberpfaffenhofen, July 1988.

Lange K, Optimization, 2004, Springer.

Madsen K, Nielsen HB, Tingleff O, Optimization With Constraints, 2004, IMM, Technical University of Denmark.
}

\keyword{ package }


