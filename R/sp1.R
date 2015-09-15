#' SP1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [60]
#' 
#' @aliases sp1 SP1
generateSP1 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("SP1 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("SP1 supports only out.dim = 2.")
  
  # FIXME unknown parameter domains
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "sp1",
    id = sprintf("sp1-%id-%id", in.dim, out.dim),
    fun = sp1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of sp1
sp1 = function(x) {
  f1 = (x[1L] - 1)^2 + (x[1L] - x[2L])^2
  f2 = (x[2L] - 3)^2 + (x[1L] - x[2L])^2
  return(c(f1, f2))
}
