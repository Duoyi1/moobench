#' MLF2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [58]
#' 
#' @aliases mlf2 MLF2
generateMLF2 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("MLF2 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("MLF2 supports only out.dim = 2.")
  
  # FIXME unknown parameter domains
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -4, upper = 4)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mlf2",
    id = sprintf("mlf2-%id-%id", in.dim, out.dim),
    fun = mlf2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mlf2
mlf2 = function(x) {
  f1 = 5 - ((x[1L]^2 + x[2L] - 11)^2 + (x[1L] + x[2L]^2 - 7)^2) / 200
  f2 = 5 - (((2 * x[1L])^2 + 2 * x[2L] - 11)^2 + ( 2 * x[1L] + (2 * x[2L])^2 - 7)^2) / 200
  return(c(f1, f2))
}