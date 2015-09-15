#' MLF1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be one.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [58]
#' 
#' @aliases mlf1 MLF1
generateMLF1 = function(in.dim = 1L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 1L)
    stop("MLF1 supports only in.dim = 1.")
  if (out.dim != 2L)
    stop("MLF1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 20)
  
  paretoSet = NULL
  
  mooFunction(
    name = "mlf1",
    id = sprintf("mlf1-%id-%id", in.dim, out.dim),
    fun = mlf1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of mlf1
mlf1 = function(x) {
  f1 = (1 + x / 20) * sin(x)
  f2 = (1 + x / 20) * cos(x)
  return(c(f1, f2))
}