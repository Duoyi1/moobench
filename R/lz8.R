#' LZ8 test function generator.
#' 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references LZ Reference Einfuegen
#' 
#' @export
#' 
generateLZ8 = function(in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  if (out.dim != 2L)
    stop("LZ8 support only out.dim = 2.")
  
  lower.bounds = rep(0, in.dim)
  upper.bounds = rep(1, in.dim)
  
  mooFunction(
    name = "lz8",
    id = sprintf("lz8-%id-$id", in.dim, out.dim),
    fun = function(x)
      evalMooFunction(lz8, x, in.dim, out.dim, lower.bounds, upper.bounds),
    in.dim = in.dim,
    out.dim = out.dim,
    lower.bounds = lower.bounds,
    upper.bounds = upper.bounds,
    pareto.set = NULL,
    pareto.front = NULL)
}


lz8 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - x[1L]^(0.5 * (1 + (3 * (j - length(x))) / (length(x) - 2)))
  }
  
  f1 = x[1L] + 2 / length(j1) * 
    (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / (sqrt(j1)))) + 2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / (sqrt(j2)))) + 2)
  return(c(f1, f2))
}
