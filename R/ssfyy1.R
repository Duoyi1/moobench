#' SSFYY1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [61]
#' 
#' @aliases ssfyy1 SSFYY1
generateSFFYY1 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("SSFYY1 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("SSFYY1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -100, upper = 100)
  
  paretoSet = NULL
  
  mooFunction(
    name = "ssfyy1",
    id = sprintf("ssfyy1-%id-%id", in.dim, out.dim),
    fun = ssfyy1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of ssfyy1
ssfyy1 = function(x) {
  f1 = sum(x^2)
  f2 = (x[1L] - 1)^2 + (x[2L] - 2)^2
  return(c(f1, f2))
}
