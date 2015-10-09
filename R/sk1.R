#' SK1 test function generator.
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
#' @aliases sk1 SK1
generateSK1 = function(in.dim = 1L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 1L)
    stop("SK1 supports only in.dim = 1.")
  if (out.dim != 2L)
    stop("SK1 supports only out.dim = 2.")
  
  # FIXME unknown parameter domains
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "sk1",
    id = sprintf("sk1-%id-%id", in.dim, out.dim),
    fun = sk1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of sk1
sk1 = function(x) {
  f1 = -x^4 - 3 * x^3 + 10 * x^2 + 10 * x + 10
  f2 = -0.5 * x^4 + 2 * x^3 + 10 * x^2 - 10 * x + 5
  return(c(f1, f2))
}