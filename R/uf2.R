#' UF2 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references Cec 09 Reference Einfuegen
#' 
#' @export
#' 
generateUF2 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("UF2 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf2",
    id = sprintf("uf2-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(lz5, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}