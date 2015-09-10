#' LZ2 test function generator.

generateLZ2 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  lz2 = function(x) {
    j = 2:length(x)
    j1 = j[j %% 2 == 1L]
    j2 = j[j %% 2 == 0L]
    
    f1 = x[1L] + 2 / length(j1) * 
      sum((x[j1] - sin(6 * pi * x[1L] + (j1 * pi) / length(x)))^2)
    f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
      sum((x[j2] - sin(6 * pi * x[1L] + (j2 * pi) / length(x)))^2)
    return(c(f1, f2))
  }
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    j = 2:in.dim
    
    des[, -1L] = t(sapply(x1, function(x) sin(6 * pi * x + (j * pi) / in.dim)))

    des
  }
  
  mooFunction(
    name = "lz2",
    id = sprintf("lz2-%id-%id", in.dim, out.dim),
    fun = function(x) lz2(x),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet)
}
