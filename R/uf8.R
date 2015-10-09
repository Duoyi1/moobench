# UF8 test function generator.

generateUF8 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    x2 = des[, 2L]
    j = 3:in.dim
    
    des[, -(1:2)] = t(sapply(seq_along(x1), function(i) 
      2 * x2[i] * sin(2 * pi * x1[i] + (j * pi) / in.dim)))
    
    des
  }
  
  paretoFront = NULL
  
  mooFunction(
    name = "uf8",
    id = sprintf("uf8-%id-%id", in.dim, out.dim),
    fun = lz6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}
