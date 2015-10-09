# MOP6 test function generator.

generateMOP6 = function(in.dim = 2L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mop6",
    id = sprintf("mop6-%id-%id", in.dim, out.dim),
    fun = mop6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mop6
mop6 = function(x) {
  f1 = x[1L]
  f2 = (1 + 10 * x[2L]) * (1 - (x[1L] / (1 + 10 * x[2L]))^2 - 
      x[1L] / (1 + 10 * x[2L]) * sin(8 * pi * x[1L]))
  return(c(f1, f2))
}