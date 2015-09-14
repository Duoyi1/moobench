#' CF2 test function generator.

generateCF2 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = lz2(x)
      tt = f[2L] + sqrt(f[1L]) - sin(2 * pi * (sqrt(f[1L]) - f[2L] + 1)) - 1
      tt / (1 + exp(4 * abs(tt))) >= 0
    }))
  
  paretoSet = NULL
  
  mooFunction(
    name = "cf2",
    id = sprintf("cf2-%id-%id", in.dim, out.dim),
    fun = lz2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
