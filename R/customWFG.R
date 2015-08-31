#' Generate a custom WFG test function with WFG toolkit.
#' 
#' @param x.max [\code{numeric}] \cr
#'   Domains.
#' @param S [\code{numeric}] \cr
#'   Scaling constants.
#' @param D [\code{numeric(1)}] \cr
#'   Distance scaling constant.
#' @param A [\code{numeric}] \cr
#'   Degeneracy constants.
#' @param trafos [\code{list}] \cr
#'   List of WFG trafos.
#' @param shapeTrafos [\code{list}] \cr
#'   List of WFG shape trafos.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' Huband, Simon ; Hingston, Phil ; Barone, Luigi ; While, Lyndon:
#' A Review of Multiobjective Test Problems and a Scalable Test Problem
#' Toolkit. In: IEEE Trans. on Evolutionary Computation 10 (2006), 
#' No. 5, pp. 477-506
#' 
#' @export

generateCustomWFG = function(x.max, S, D, A, trafos, shapeTrafos) {
  assertNumber(D, lower = 0)
  assertNumeric(A, lower = 0, upper = 1)
  assertNumeric(S, lower = 0)
  out.dim = length(shapeTrafos)
  
  custumWFG = function(x) {
    x = x / x.max
    for (i in seq_along(trafos)) 
      x = trafos[[i]](x)
    x = c(max(x[out.dim], A) * (x[-out.dim] - 0.5) + 0.5, x[out.dim])
    sapply(seq_along(shapeTrafos), function(i) D * x[out.dim] + S[i] * shapeTrafos[[i]](x[-out.dim]))
  }
  
  custumWFG = addClasses(custumWFG, "custumWFGFun")
  return(custumWFG)
}