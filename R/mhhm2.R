#' MHHM2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Must be two.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be three.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [57]
#' 
#' @aliases mhhm2 MHHM2
generateMHHM2 = function(in.dim = 2L, out.dim = 3L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (in.dim != 2L)
    stop("MHHM2 supports only in.dim = 2.")
  if (out.dim != 3L)
    stop("MHHM2 supports only out.dim = 3.")
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mhhm2",
    id = sprintf("mhhm2-%id-%id", in.dim, out.dim),
    fun = mhhm2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mhhm2
mhhm2 = function(x) {
  f1 = (x[1L] - 0.8)^2 + (x[2L] - 0.6)^2
  f2 = (x[1L] - 0.85)^2 + (x[2L] - 0.7)^2
  f3 = (x[1L] - 0.9)^2 + (x[2L] - 0.6)^2
  return(c(f1, f2, f3))
}