#' DTLZ1 test function generator.

generateDTLZ1 = function(in.dim = 30L, out.dim = 2L) {

  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  dtlz1 = function(x, out.dim) {
    x.head = x[seq_len(out.dim - 1L)]
    x.tail = x[out.dim:length(x)] - 0.5
    
    rev(0.5 * c(1, cumprod(x.head)) * c(1 - x.head, 1) * 
        (1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))))
  }
  
  paretoSet = NULL
  
  mooFunction(
    name = "dtlz1",
    id = sprintf("dtlz1-%id-%id", in.dim, out.dim),
    fun = dtlz1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
