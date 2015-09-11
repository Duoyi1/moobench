#' DTLZ4 test function generator.

generateDTLZ4 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  dtlz4 = function(x, out.dim) {
    x.head = x[seq_len(out.dim - 1L)]^100 * 0.5 * pi
    x.tail = x[out.dim:length(x)] - 0.5
    
    rev((1 + sum(x.tail^2)) * c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
  }
  
  paretoSet = NULL
  
  mooFunction(
    name = "dtlz4",
    id = sprintf("dtlz4-%id-%id", in.dim, out.dim),
    fun = dtlz4,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
