% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getter_mooFunction.R
\name{getter_mooFunction}
\alias{getId}
\alias{getInDim}
\alias{getName}
\alias{getOutDim}
\alias{getParamSet}
\alias{getter_mooFunction}
\title{Wrappers to get attributes from a mooFunction}
\usage{
getParamSet(f)

getName(f)

getId(f)

getInDim(f)

getOutDim(f)
}
\arguments{
\item{f}{[\code{function}] \cr
A \code{\link{mooFunction}}.}
}
\description{
\code{getParamSet} Simple wrapper for \code{attributes(f)$param.set}, returns a \code{\link[ParamHelpers]{ParamSet}} . \cr
\code{getName} Simple wrapper for \code{attributes(f)$name}, returns a \code{character(1)}. \cr
\code{getId} Simple wrapper for \code{attributes(f)$id}, returns a \code{character(1)}. \cr
\code{getInDim} Simple wrapper for \code{attributes(f)$in.dim}, returns a \code{integer(1)}. \cr
\code{getOutDim} Simple wrapper for \code{attributes(f)$out.dim}, returns a \code{integer(1)}. \cr
}

