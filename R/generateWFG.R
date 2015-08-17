#' WFG test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which WFG function? Valid values are 1, 2, ..., 9
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @export

generateWFG = function(id, in.dim, out.dim) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greater than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:9)
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  fun = switch(id,
    wfg1, 
    wfg2,
    wfg3,
    wfg4,
    wfg5,
    wfg6,
    wfg7,
    wfg8,
    wfg9
  )
  
  mooFunction(
    name = sprintf("wfg%i", id),
    id = sprintf("wfg%i-%id-$id", id, in.dim, out.dim),
    # Note: fun.args is a list here
    fun = function(x) fun(x, out.dim = out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    pareto.set = NULL,
    pareto.front = NULL)
}

# definition of wfg1
wfg1 = function(x, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = length(x)
  z.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(c(replicate(out.dim - 1, list(name = "convex"), simplify = FALSE), 
    list(list(name = "mixed", params = list(alpha = 1, A = 5L)))), out.dim = out.dim)
  
  trafos = makeWFGTrafos(list(
    list(name = "identity", ids = 1:k), list(name = "b_linear", ids = (k + 1):n, params = list(A = 0.35)),
    list(name = "identity", ids = 1:k), list(name = "b_flat", ids = (k + 1):n, params = list(A = 0.8, B = 0.75, C = 0.85)),
    list(name = "b_poly", ids = 1:n, params = list(alpha = 0.02)),
    list(name = "r_sum", ids = 1:(out.dim - 1), params = list(w = ))))  
  
  generateCustomWFG(z.max, S, D, A, trafos, shapeTrafos)
}