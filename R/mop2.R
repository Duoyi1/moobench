# MOP2 test function generator.

generateMOP2 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -4, upper = 4)
  
  paretoSet = NULL
  
  mooFunction(
    name = "mop2",
    id = sprintf("mop2-%id-%id", in.dim, out.dim),
    fun = mop2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of mop2
mop2 = function(x) {
  f1 = 1 - exp(-sum((x - 1 / sqrt(length(x)))^2))
  f2 = 1 - exp(-sum((x + 1 / sqrt(length(x)))^2))
  return(c(f1, f2))
}