#' Wrappers to get pareto set and front from a mooFunction
#'
#' \code{getParetoSet} Simple wrapper for \code{attributes(f)$paretoSet(n)}, returns a \code{vector(n)}. \cr
#' \code{getParetoFront} Simple wrapper for calculate the pareto front, returns a \code{vector(n)}. \cr
#'
#' @param f [\code{function}] \cr
#'   A \code{\link{mooFunction}}.
#' @param n [\code{integer(1)}] \cr.
#'   Number of points.

#' @rdname get_pareto
#' @export
getParetoSet = function(f, n) {
  UseMethod("getParetoSet")
}

#' @export
getParetoSet.mooFunction = function(f, n = 100 * getOutDim(f)) {
  fun = attributes(f)$paretoSet
  if (is.null(fun)) {
    warning("No Pareto set definition for f available.")
    return(matrix(NA, ncol = getInDim(f), nrow = 0L))
  }
  else
    return(fun(n))
}


#' @rdname get_pareto
#' @export
getParetoFront = function(f, n) {
  UseMethod("getParetoFront")
}

#' @export
getParetoFront.mooFunction = function(f, n = 100 * getOutDim(f)) {
  set = getParetoSet(f, n)
  front = t(apply(set, 1, f))
  front = front[order(front[, 1L]), ]
  front
}