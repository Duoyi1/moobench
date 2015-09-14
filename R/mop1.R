# MOP1 test function generator.

generateMOP1 = function(in.dim = 1L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -10e5, upper = 10e5)
  
  paretoSet = NULL
  
  mooFunction(
    name = "mop1",
    id = sprintf("mop1-%id-%id", in.dim, out.dim),
    fun = mop1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of mop1
mop1 = function(x) {
  f1 = x^2
  f2 = (x - 2)^2
  return(c(f1, f2))
}