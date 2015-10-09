#' Far1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [48]
#' 
#' @aliases far1 Far1
generateFar1 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("Far1 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("Far1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -1, upper = 1)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "far1",
    id = sprintf("far1-%id-%id", in.dim, out.dim),
    fun = far1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of far1
far1 = function(x) {
  x1 = x[1L]
  x2 = x[2L]
  f1 = -2 * exp(15 * (-(x1 - 0.1)^2 - x2^2)) -
    exp(20 * (-(x1 - 0.6)^2 - (x2 - 0.6)^2)) +
    exp(20 * (-(x1 + 0.6)^2 - (x2 - 0.6)^2)) +
    exp(20 * (-(x1 - 0.6)^2 - (x2 + 0.6)^2)) +
    exp(20 * (-(x1 + 0.6)^2 - (x2 + 0.6)^2))
  f2 = 2 * exp(20 * (-x1^2 - x2^2)) +
    exp(20 * (-(x1 - 0.4)^2 - (x2 - 0.6)^2)) -
    exp(20 * (-(x1 + 0.5)^2 - (x2 - 0.7)^2)) -
    exp(20 * (-(x1 - 0.5)^2 - (x2 + 0.7)^2)) +
    exp(20 * (-(x1 + 0.4)^2 - (x2 + 0.8)^2))
  return(c(f1, f2))
}