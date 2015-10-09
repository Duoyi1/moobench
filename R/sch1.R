#' Sch1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be one.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [34], [39]
#' 
#' @aliases sch1 SCH1
generateSch1 = function(in.dim = 1L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 1L)
    stop("Sch1 supports only in.dim = 1.")
  if (out.dim != 2L)
    stop("Sch1 supports only out.dim = 2.")
  
  # FIXME unknown parameter domains
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -6, upper = 6)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "sch1",
    id = sprintf("sch1-%id-%id", in.dim, out.dim),
    fun = sch1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of sch1
sch1 = function(x) {
  if (x <= 1)
    f1 = -x
  if (1 < x && x <= 3)
    f1 = -2 + x
  if (3 < x && x <= 4)
    f1 = 4 - x
  if (4 < x)
    f1 = -4 + x
  
  f2 = (x - 5)^2
  return(c(f1, f2))
}