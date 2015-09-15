#' TKLY1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be four.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [62]
#' 
#' @aliases tkly1 TKLY1
generateTKLY1 = function(in.dim = 4L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 4L)
    stop("TKLY1 supports only in.dim = 4.")
  if (out.dim != 2L)
    stop("TKLY1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, upper = c(0.1, 0, 0, 0),
    lower = rep(1, in.dim))
  
  paretoSet = NULL
  
  mooFunction(
    name = "tkly1",
    id = sprintf("tkly1-%id-%id", in.dim, out.dim),
    fun = tkly1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of tkly1
tkly1 = function(x) {
  f1 = x[1L]
  f2 = 1 / x[1L] * prod(2 - exp(-((x[-1L] - 0.1) / 0.004)^2) - 
      0.8 * exp(-((x[-1L] - 0.9) / 0.4)^2))
  return(c(f1, f2))
}