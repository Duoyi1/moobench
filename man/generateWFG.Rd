% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/wfg.R
\name{generateWFG}
\alias{WFG}
\alias{generateWFG}
\alias{wfg}
\title{WFG test function generator.}
\usage{
generateWFG(id, in.dim, out.dim, k)
}
\arguments{
\item{id}{[\code{integer(1)}] \cr
Which WFG function? Valid values are 1, 2, ..., 9}

\item{in.dim}{[\code{integer(1)}] \cr
Size of parameter space.}

\item{out.dim}{[\code{integer(1)}] \cr
Size of target space.}

\item{k}{[\code{integer(1)}] \cr
Number position-related parameters.}
}
\value{
A \code{mooFunction}.
}
\description{
WFG test function generator.
}
\references{
Huband, Simon ; Hingston, Phil ; Barone, Luigi ; While, Lyndon:
A Review of Multiobjective Test Problems and a Scalable Test Problem
Toolkit. In: IEEE Trans. on Evolutionary Computation 10 (2006),
No. 5, pp. 477-506
}

