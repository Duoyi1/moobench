#' IM1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [52]
#' 
#' @aliases im1 IM1
generateIM1 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("IM1 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("IM1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = c(1, 1), upper = c(4, 2))
  
  paretoSet = NULL
  
  mooFunction(
    name = "im1",
    id = sprintf("im1-%id-%id", in.dim, out.dim),
    fun = im1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of im1
im1 = function(x) {
  f1 = 2 * sqrt(x[1L])
  f2 = x[1L] * (1 - x[2L]) + 5
  return(c(f1, f2))
}