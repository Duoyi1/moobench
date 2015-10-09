# ZDT4 test function generator.

generateZDT4 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = c(0, rep(-5, in.dim - 1)), 
    upper = c(1, rep(5, in.dim - 1)))
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    mat = matrix(0, nrow = n, ncol = in.dim - 1L)
    des[, -1L] = mat
    des
  }
  
  paretoFront = NULL
  
  mooFunction(
    name = "zdt4",
    id = sprintf("zdt4-%id-%id", in.dim, out.dim),
    fun = zdt4,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of zdt4
zdt4 = function(x) {
  f1 = x[1L]
  m = length(x)
  g = 1 + 10 * (m - 1) + sum(x[-1L]^2 - 10 * cos(4 * pi * x[-1L]))
  f2 = g * (1 - sqrt(f1 / g))
  return(c(f1, f2))
}
