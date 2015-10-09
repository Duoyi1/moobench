# MOP4 test function generator.

generateMOP4 = function(in.dim = 3L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = -5, upper = 5)
  
  paretoSet = NULL
  
  paretoFront = NULL
  
  mooFunction(
    name = "mop4",
    id = sprintf("mop4-%id-%id", in.dim, out.dim),
    fun = mop4,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of mop4
mop4 = function(x) {
  f1 = sum(-10 * exp(-0.2 * sqrt(x[-3L]^2 + x[-1L]^2)))
  f2 = sum(abs(x)^0.8 + 5 * sin(x^3))
  return(c(f1, f2))
}