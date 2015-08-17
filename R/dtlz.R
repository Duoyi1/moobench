#' DTLZ test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which DTLZ function? Valid values are 1, 2, ..., 6
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references DTLZ Reference Einfuegen
#' 
#' @export
#' 
generateDTLZ = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:6)
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  fun = switch(id,
    dtlz1, 
    dtlz2,
    dtlz3,
    dtlz4,
    dtlz5,
    dtlz6
    )
  
  mooFunction(
    name = sprintf("dtlz%i", id),
    id = sprintf("dtlz%i-%id-$id", id, in.dim, out.dim),
    # Note: fun.args is a list here
    fun = function(x) fun(x, out.dim = out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    pareto.set = NULL,
    pareto.front = NULL)
}


# definition of dtlz1-6
dtlz1 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)]
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev(0.5 * c(1, cumprod(x.head)) * c(1 - x.head, 1) * 
      (1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))))
}

dtlz2 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)] * 0.5 * pi
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev((1 + sum(x.tail^2)) * c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
}

dtlz3 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)] * 0.5 * pi
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev((1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))) * 
      c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
}

dtlz4 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)]^100 * 0.5 * pi
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev((1 + sum(x.tail^2)) * c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
}

dtlz5 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)]
  x.tail = x[out.dim:length(x)] - 0.5
  
  g = sum(x.tail)^0.1
  theta = numeric(out.dim - 1L)
  if (out.dim > 1L) 
    theta[1L] = x.head[1L] * 0.5 * pi
  if (out.dim == 3L) 
    theta[2L] = (pi / (4 * (1 + g))) * (1 + 2 * g * x.head[2L]) * 0.5 * pi
  if (out.dim > 3L)
    theta[2:(out.dim - 1L)] = (pi / (4 * (1 + g))) * (1 + 2 * g * x.head[2:(out.dim - 1L)]) * 0.5 * pi
  
  rev((1 + g) * c(sin(theta), 1) * c(1, cumprod(cos(theta))))
}

dtlz6 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)]
  x.tail = x[out.dim:length(x)]
  
  g = 1 + (9 / length(x.tail)) * sum(x.tail)
  h = length(x) - sum((x.head / (1 + g)) * (1 + sin(3 * pi * x.head)))
  fm = (1 + g) * h
  
  c(x.head, fm)
}
