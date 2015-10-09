# ZDT3 test function generator.

generateZDT3 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    # Sample some more. We have a disconnected front an have to remove some
    # dominated points later
    des = generateDesign(par.set = param.set, n = 10 * n)
    des = des[order(des[, 1L]), ]
    
    mat = matrix(0, nrow = 10 * n, ncol = in.dim - 1L)
    des[, -1L] = mat
    
    des = des[!is_dominated(apply(des, 1, zdt3)), ]
    des = des[sample(nrow(des), n), ]
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    des
  }
  
  paretoFront = NULL
  
  mooFunction(
    name = "zdt3",
    id = sprintf("zdt3-%id-%id", in.dim, out.dim),
    fun = zdt3,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of zdt3
zdt3 = function(x) {
  f1 = x[1L]
  g = 1 + 9 * mean(x[-1L])
  f2 = g * (1 - sqrt(f1 / g) - (f1 / g) * sin(10 * pi * f1))
  return(c(f1, f2))
}