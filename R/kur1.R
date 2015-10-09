#' Kur1 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Must be two.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' wfg [37]
#' 
#' @aliases kur1 Kur1
generateKur1 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim != 2L)
    stop("Kur1 supports only out.dim = 2.")
  
  # FIXME unknown parameter domains
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "kur1",
    id = sprintf("kur1-%id-%id", in.dim, out.dim),
    fun = kur1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of kur1
kur1 = function(x) {
  f1 = sum(-10 * exp(-0.2 * sqrt(x[-length(x)]^2 + x[-1L]^2)))
  f2 = sum(abs(x)^0.8 + 5 * sin(x)^3)
  return(c(f1, f2))
}