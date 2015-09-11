#' DTLZ3 test function generator.

generateDTLZ3 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  dtlz3 = function(x, out.dim) {
    x.head = x[seq_len(out.dim - 1L)] * 0.5 * pi
    x.tail = x[out.dim:length(x)] - 0.5
    
    rev((1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))) * 
        c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
  }
  
  paretoSet = NULL
  
  mooFunction(
    name = "dtlz3",
    id = sprintf("dtlz3-%id-%id", in.dim, out.dim),
    fun = dtlz3,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
