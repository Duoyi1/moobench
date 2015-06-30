#' UF7 test function generator.
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
generateUF7 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("UF7 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "uf7",
    id = sprintf("uf7-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(uf7, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


uf7 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L]^0.2 + 2 / length(j1) * sum(y(j1)^2)
  f2 = 1 - x[1L]^0.2 + 2 / length(j2) * sum(y(j2)^2)
  return(c(f1, f2))
}