#' @title Example for creating a custum WFG test Problem.
#'
#' @description
#' \strong{Step I - Specifying values for the underlying formalism}
#' Including scaling constants and parameter domains.
#'
#' \strong{Step II - Specifying the shape functions}
#' 
#' 
#' \strong{Step III - Specifying transition vectors}
#' 
#' 
#' @name example_wfg
NULL

example_wfg = function(in.dim, out.dim, k) {
  S = 2 * 1:out.dim
  D = 1
  A = rep(1, out.dim - 1)
  n = in.dim
  x.max = 2 * 1:n
  
  shapeTrafos = makeWFGShapeTrafo(arg = c(replicate(out.dim - 1L, list(name = "convex"), simplify = FALSE), 
    list(list(name = "mixed", params = list(alpha = 1, A = 5L)))))
  
  trafo1 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k),
    list(name = "s_linear", ids = (k + 1):n, params = list(A = 0.35))
  ))
  trafo2 = makeWFGTrafo(list(
    list(name = "identity", ids = 1:k), 
    list(name = "b_flat", ids = (k + 1):n, params = list(A = 0.8, B = 0.75, C = 0.85))
  ))
  trafo3 = makeWFGTrafo(list(
    list(name = "b_poly", ids = 1:n, params = list(alpha = 0.02))
  ))
  
  rIds = split(1:k, rep(1:(out.dim - 1), each = k / (out.dim - 1)))

  trafo4 = makeWFGTrafo(c(lapply(1:(out.dim - 1), function(i) {
    list(name = "r_sum", ids = rIds[[i]], params = list(w = 2 * rIds[[i]]))
  }),
    list(list(name = "r_sum", ids = (k + 1):n, params = list(w = 2 * (k + 1):n)))
  ))
  
  trafos = list(trafo1, trafo2, trafo3, trafo4)
  
  generateCustomWFG(x.max, S, D, A, trafos, shapeTrafos)
}