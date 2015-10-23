# LZ5 test function generator.

generateLZ5 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim,
    lower = c(0, rep(-1, in.dim -1)), upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    j = 2:in.dim
    j1 = j[j %% 2 == 1L]
    j2 = j[j %% 2 == 0L]
    
    tmp1 = sapply(x1, function(x) (0.3 * x^2 * cos(24 * pi * x + (4 * j1 * pi) / in.dim) +
        0.6 * x) * cos(6 * pi * x + (j1 * pi) / in.dim))
    tmp2 = sapply(x1, function(x) (0.3 * x^2 * cos(24 * pi * x + (4 * j2 * pi) / in.dim)
      + 0.6 * x) * sin(6 * pi * x + (j2 * pi) / in.dim))
    
    if (is.vector(tmp1))
      des[, j1] = tmp1
    else
      des[, j1] = t(tmp1)
    
    if (is.vector(tmp2))
      des[, j2] = tmp2
    else
      des[, j2] = t(tmp2)
    
    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    f1 = runif(n)
    f2 = 1 - sqrt(f1)
    
    des = cbind(f1, f2)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des
  }
  
  mooFunction(
    name = "lz5",
    id = sprintf("lz5-%id-%id", in.dim, out.dim),
    fun = lz5,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of lz5
lz5 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - (0.3 * x[1L]^2 * cos(24 * pi * x[1L] + (4 * j1 * pi) / length(x)) + 0.6 * x[1L]) * 
        cos(6 * pi * x[1L] + (j1 * pi) / length(x) ))^2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum((x[j2] - (0.3 * x[1L]^2 * cos(24 * pi * x[1L] + (4 * j2 * pi) / length(x)) + 0.6 * x[1L]) * 
        sin(6 * pi * x[1L] + (j2 * pi) / length(x) ))^2)
  return(c(f1, f2))
}
