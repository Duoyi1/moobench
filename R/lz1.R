# LZ1 test function generator.

generateLZ1 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    j = 2:in.dim
    
    des[, -1L] = t(sapply(x1, function(x) x^(0.5 * (1 + (3 * (j - 2)) / (in.dim - 2)))))

    des
  }
  
  mooFunction(
    name = "lz1",
    id = sprintf("lz1-%id-%id", in.dim, out.dim),
    fun = lz1,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}

# Definiton of lz1
lz1 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - x[1L]^(0.5 * (1 + (3 * (j1 - 2)) / (length(x) - 2))))^2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum((x[j2] - x[1L]^(0.5 * (1 + (3 * (j2 - 2)) / (length(x) - 2))))^2)
  return(c(f1, f2))
}