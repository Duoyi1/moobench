# LZ9 test function generator.

generateLZ9 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim,
    lower = c(0, rep(-1, in.dim -1)), upper = 1)
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    j = 2:in.dim
    
    des[, -1L] = t(sapply(x1, function(x) sin(6 * pi * x + (j * pi) / in.dim)))

    des
  }
  
  paretoFront = function(n = out.dim * 100L) {
    f1 = runif(n)
    f2 = 1 - f1^2
    
    des = cbind(f1, f2)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des
  }
  
  mooFunction(
    name = "lz9",
    id = sprintf("lz9-%id-%id", in.dim, out.dim),
    fun = lz9,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of lz9
lz9 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - sin(6 * pi * x[1L] + (j1 * pi) / length(x)))^2)
  f2 = 1 - x[1L]^2 + 2 / length(j2) * 
    sum((x[j2] - sin(6 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  return(c(f1, f2))
}