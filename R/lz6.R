# LZ6 test function generator.

generateLZ6 = function(in.dim = 30L, out.dim = 2L) {
  
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
  
  paretoFront = function(n = out.dim * 100L) {
    des = matrix(runif(3 * n), nrow = n, ncol = 3L)
    des = des / rowSums(des)
    des[, 1L] = sqrt(des[, 1L])
    des[, 2L] = sqrt(des[, 2L])
    des[, 3L] = (des[, 3L])^(1 / 3)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    des
  }
  
  mooFunction(
    name = "lz6",
    id = sprintf("lz6-%id-%id", in.dim, out.dim),
    fun = lz6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

# Definiton of lz6
lz6 = function(x) {
  j = 3:length(x)
  j1 = j[j %% 3 == 1L]
  j2 = j[j %% 3 == 2L]
  j3 = j[j %% 3 == 0L]
  
  f1 = cos(0.5 * x[1L] * pi) * cos(0.5 * x[2L] * pi) + 2 / length(j1) * 
    sum((x[j1] - 2 * x[2L] * sin(2 * pi * x[1L] + (j1 * pi) / length(x)))^2)
  f2 = cos(0.5 * x[1L] * pi) * sin(0.5 * x[2L] * pi) + 2 / length(j2) * 
    sum((x[j2] - 2 * x[2L] * sin(2 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  f3 = sin(0.5 * x[1L] * pi) + 2 / length(j3) * 
    sum((x[j3] - 2 * x[2L] * sin(2 * pi * x[1L] + (j3 * pi) / length(x)))^2)
  
  return(c(f1, f2, f3))
}