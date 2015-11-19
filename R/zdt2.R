# ZDT2 test function generator.

generateZDT2 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    mat = matrix(0, nrow = n, ncol = in.dim - 1L)
    des[, -1L] = mat
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    f1 = runif(n)
    f2 = 1 - f1^2
    
    des = cbind(f1, f2)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    as.data.frame(des)
  }
  
  mooFunction(
    name = "zdt2",
    id = sprintf("zdt2-%id-%id", in.dim, out.dim),
    fun = zdt2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of zdt2
zdt2 = function(x) {
  f1 = x[1L]
  g = 1 + 9 * mean(x[-1L])
  f2 = g * (1 - (f1 / g)^2)
  return(c(f1, f2))
}