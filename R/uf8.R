#' UF8 test function generator.
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
generateUF8 = function(in.dim = 30, out.dim = 3) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 3L)
    stop("UF8 support only out.dim = 3.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf8",
    id = sprintf("uf8-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(lz6, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}
