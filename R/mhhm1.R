#' MHHM1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be one.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be three.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [57]
#' 
#' @aliases mhhm1 MHHM1
generateMHHM1 = function(in.dim = 1L, out.dim = 3L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 1L)
    stop("MHHM1 supports only in.dim = 1.")
  if (out.dim != 3L)
    stop("MHHM1 supports only out.dim = 3.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "mhhm1",
    id = sprintf("mhhm1-%id-%id", in.dim, out.dim),
    fun = mhhm1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of mhhm1
mhhm1 = function(x) {
  f1 = (x - 0.8)^2
  f2 = (x - 0.85)^2
  f3 = (x - 0.9)^2
  return(c(f1, f2, f3))
}
