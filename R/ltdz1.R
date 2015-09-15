#' LTDZ1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be three.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be three.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [55]
#' 
#' @aliases ltdz1 LTDZ1
generateLTDZ1 = function(in.dim = 3L, out.dim = 3L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 3L)
    stop("LTDZ1 supports only in.dim = 3.")
  if (out.dim != 3L)
    stop("LTDZ1 supports only out.dim = 3.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "ltdz1",
    id = sprintf("ltdz1-%id-%id", in.dim, out.dim),
    fun = ltdz1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of ltdz1
ltdz1 = function(x) {
  f1 = 3 - (1 + x[3L]) * cos(x[1L] * pi / 2) * cos(x[2L] * pi / 2)
  f2 = 3 - (1 + x[3L]) * cos(x[1L] * pi / 2) * sin(x[2L] * pi / 2)
  f3 = 3 - (1 + x[3L]) * sin(x[1L] * pi / 2)
  return(c(f1, f2, f3))
}