#' SSFYY2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be one.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [61]
#' 
#' @aliases ssfyy2 SSFYY2
generateSSFYY2 = function(in.dim = 1L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 1L)
    stop("SSFYY2 supports only in.dim = 1.")
  if (out.dim != 2L)
    stop("SSFYY2 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -100, upper = 100)
  
  paretoSet = NULL
  
  mooFunction(
    name = "ssfyy2",
    id = sprintf("ssfyy2-%id-%id", in.dim, out.dim),
    fun = ssfyy2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of ssfyy2
ssfyy2 = function(x) {
  f1 = 10 + x^2 - 10 * cos(x * pi / 2)
  f2 = (x - 4)^2
  return(c(f1, f2))
}