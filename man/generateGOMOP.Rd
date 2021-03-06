% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/gomop.R
\name{generateGOMOP}
\alias{GOMOP}
\alias{generateGOMOP}
\alias{gomop}
\title{GOMOP functions}
\usage{
generateGOMOP(in.dim = 30L, out.dim, soobench.funs = list())
}
\arguments{
\item{in.dim}{[\code{integer(1)}] \cr
Size of parameter space. Every soobench-function must have this in.dim}

\item{out.dim}{[\code{integer(1)}] \cr
Size of target space. Is automatically set to length of soobench.funs.}

\item{soobench.funs}{[\code{list}] \cr
List of \code{\link[soobench]{soo_function}}.}
}
\value{
A \code{mooFunction}.
}
\description{
GOMOP functions are multicrit test functions that are composed of multiple
single crit test functions. Here we allow only soobench functions as single
crit functions. Note that all input parameter for the GOMOP function have the
bounds (0, 1) and are stretched to the bounds of the respective soobench functions.
}
\references{
Reference Einfuegen
}

