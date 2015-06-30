#' UF3 test function generator.
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
generateUF3 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("UF3 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf3",
    id = sprintf("uf3-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(lz8, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}