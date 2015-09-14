#' ZDT5 test function generator.

generateZDT5 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet( 
    makeIntegerVectorParam(id = "x", len = in.dim, lower = 0, upper = 1)
  )
  
  paretoSet = NULL
  
  mooFunction(
    name = "zdt5",
    id = sprintf("zdt5-%id-%id", in.dim, out.dim),
    fun = zdt5,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of zdt5
zdt5 = function(x) {
  f1 = 1 + x[1L]
  m = length(x)
  
  v = function(x) {
    if (sum(x) < 5)
      return(2 + sum(x))
    else
      return(1)
  }
  
  g = v(x[-1L])
  f2 = g / f1
  return(c(f1, f2))
}
