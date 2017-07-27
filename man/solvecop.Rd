\name{solvecop}
\Rdversion{1.0}
\alias{solvecop}
\title{Solve a Constrained Optimization Problem}

\description{
Solve a constrained optimization problem with a linear, quadratic, or rational objective function, and linear, quadratic, rational, and boundary constraints.}

\usage{
solvecop(op, solver="default", make.definite=FALSE, X=NULL, quiet=FALSE, ...)
}

\arguments{
\item{op}{An optimization problem, usually created with function \link{cop}.}
\item{solver}{Character string with the name of the solver. Available solvers are \code{"alabama"}, \code{"cccp"}, \code{"cccp2"}, \code{"csdp"}, and \code{"slsqp"}. The default means that the solver is chosen automatically. The solvers are described in the Details section.
}
\item{make.definite}{Logical variable indicating whether non-positive-semidefinite matrices should be approximated by positive-definite matrices. This is always done for solvers that are known not to convergue otherwise.
}
\item{X}{Starting vector of parameter values (not needed). Any initial vector, even those violating linear inequality constraints, may be specified. Ignored by solvers \code{"cccp"} and \code{"csdp"}. For \code{"slsqp"} the lower and upper bounds must not be violated.}
\item{quiet}{Logical variable indicating whether output to console should be switched off.}
\item{...}{Tuning parameters of the solver. The available parameters depend on the solver and will be printed when the function is used with \code{quiet=FALSE}. In section Details it is mentioned where descriptions of these parameters can be found.}
}

\details{
Solve a constrained optimization problem with a linear, quadratic, or rational objective function, and linear, quadratic, rational, and boundary constraints.

\bold{Solver}

 \code{"alabama"}: The augmented lagrangian minimization algorithm \link[alabama]{auglag} from package \code{alabama} is called.
 The method combines the objective function and a penalty for each constraint  into a single function. This modified objective function is then passed to another optimization algorithm with no constraints. If the constraints are violated by the solution of this sub-problem, then the size of the penalties is increased and the process is repeated. The default methods for the uncontrained optimization in the inner loop is the quasi-Newton method called \code{BFGS}.
Tuning parameters used for the outer loop are described in the details section of the help page of function \link[alabama]{auglag}. Tuning parameters used for the inner loop are described in the details section of the help page of function \link[stats]{optim}.
 
\code{"cccp"} and \code{"cccp2"}: Function \link[cccp]{cccp} from package \code{cccp} for solving cone constrained convex programs is called. For solver \code{"cccp"}, quadratic constraints are converted into second order cone constraints, which requires to approximate non-positive-semidefinite matrices by positive-definite matrices. For solver \code{"cccp2"}, quadratic constraints are defined by functions. The implemented algorithms are partially ported from CVXOPT. Tuning parameters  are those from function \link[cccp]{ctrl}. 

 \code{"csdp"}: The problem is reformulated as a semidefinite programming problem and solved with the 'CSDP' library.
 This requires to approximate non-positive-semidefinite matrices by positive-definite matrices.
  This solver is not suitable when the objective is to minimize a rational function, but rational constraints can be used.
 Tuning parameters are described in the CSDP User's Guide: \url{https://projects.coin-or.org/Csdp/export/49/trunk/doc/csdpuser.pdf}.
 
 \code{"slsqp"}: The sequential (least-squares) quadratic programming (SQP) algorithm \link[nloptr]{slsqp} for  gradient-based optimization from package \code{nloptr}. The algorithm optimizes successive second-order (quadratic/least-squares) approximations of the objective function, with first-order (affine) approximations of the constraints.  Available parameters are described in \link[nloptr]{nl.opts}

}

\value{
A list with the following components:

\item{x}{Named numeric vector with parameters optimizing the objective function while satisfying constraints, if convergence is successful.}
\item{solver}{Name of the solver used for optimization.}
\item{status}{Message indicating type of convergence as reported by the solver.}
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

hist(res$x,breaks=50,xlim=c(0,0.5))

Evaluation <- validate(mycop, res)

Evaluation$summary

Evaluation$info

Evaluation$obj.fun

Evaluation$var

Evaluation$var$Breeding.Value

}


\author{Robin Wellmann}
