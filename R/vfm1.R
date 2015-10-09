#' VFM1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be three.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [38]
#' 
#' @aliases vfm1 VFM1
generateVFM1 = function(in.dim = 2L, out.dim = 3L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("VFM1 supports only in.dim = 2.")
  if (out.dim != 3L)
    stop("VFM1 supports only out.dim = 3.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -2, upper = 2)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "vfm1",
    id = sprintf("vfm1-%id-%id", in.dim, out.dim),
    fun = vfm1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of vfm1
vfm1 = function(x) {
  f1 = x[1L]^2 + (x[2L] - 1)^2
  f2 = x[1L]^2 + (x[2L] + 1)^2 + 1
  f3 = (x[1L] - 1)^2 + x[2L]^2 + 2
  return(c(f1, f2, f3))
}
