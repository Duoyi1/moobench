#' VU2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [64]
#' 
#' @aliases vu2 VU2
generateVU2 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("VU2 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("VU2 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -3, upper = 3)
  
  paretoSet = NULL
  
  mooFunction(
    name = "vu2",
    id = sprintf("vu2-%id-%id", in.dim, out.dim),
    fun = vu2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of vu2
vu2 = function(x) {
  f1 = sum(x^2) + 1
  f2 = x[1L]^2 + 2 * x[2L] - 1
  return(c(f1, f2))
}
