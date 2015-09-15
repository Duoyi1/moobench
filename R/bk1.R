#' BK1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [45]
#' 
#' @aliases bk1 BK1
generateBK1 = function(in.dim = 2L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("BK1 supports only in.dim = 2.")
  if (out.dim != 2L)
    stop("BK1 supports only out.dim = 2.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -5, upper = 10)
  
  paretoSet = NULL
  
  mooFunction(
    name = "bk1",
    id = sprintf("bk1-%id-%id", in.dim, out.dim),
    fun = bk1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of bk1
bk1 = function(x) {
  f1 = sum(x^2)
  f2 = sum((x - 5)^2)
  return(c(f1, f2))
}
