#' ZLT1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. 
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. 
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [11]
#' 
#' @aliases zlt1 ZLT1
generateZLT1 = function(in.dim = 20L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -1000, upper = 1000)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "zlt1",
    id = sprintf("zlt1-%id-%id", in.dim, out.dim),
    fun = function(x) zlt1(x, out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of zlt1
zlt1 = function(x, out.dim) {
  sapply(1:out.dim, function(m) (x[m] - 1)^2 + sum(x[-m]^2))
}