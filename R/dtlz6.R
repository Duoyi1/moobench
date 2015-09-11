#' DTLZ6 test function generator.

generateDTLZ6 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  mooFunction(
    name = "dtlz6",
    id = sprintf("dtlz6-%id-%id", in.dim, out.dim),
    fun = dtlz6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of dtlz6
dtlz6 = function(x, out.dim) {
  x.head = x[seq_len(out.dim - 1L)]
  x.tail = x[out.dim:length(x)]
  
  g = 1 + (9 / length(x.tail)) * sum(x.tail)
  h = length(x) - sum((x.head / (1 + g)) * (1 + sin(3 * pi * x.head)))
  fm = (1 + g) * h
  
  c(x.head, fm)
}