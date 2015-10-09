#' LRS1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [54]
#' 
#' @aliases lrs1 LRS1
generateLRS1 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("LRS1 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("LRS1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -50, upper = 50)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "lrs1",
    id = sprintf("lrs1-%id-%id", in.dim, out.dim),
    fun = lrs1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of lrs1
lrs1 = function(x) {
  f1 = x[1L]^2 + x[2L]^2
  f2 = (x[1L] + 2)^2 + x[2L]^2
  return(c(f1, f2))
}