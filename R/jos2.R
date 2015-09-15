#' JOS2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [53]
#' 
#' @aliases jos2 JOS2
generateJOS2 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 2L)
    stop("JOS2 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "jos2",
    id = sprintf("jos2-%id-%id", in.dim, out.dim),
    fun = jos2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of jos2
jos2 = function(x) {
  f1 = x[1L]
  g = 1 + 9 * sum(x[-1L] / (length(x) - 1))
  f2 = g * (1 - (f1 / g)^0.25 - (f1 / g)^4)
  return(c(f1, f2))
}