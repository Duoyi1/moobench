# UF8 test function generator.

generateUF8 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeNumericParamSet(id = "x", len = in.dim,
    lower = c(0, 0, rep(-2, in.dim - 2)), upper = c(1, 1, rep(2, in.dim - 2)))
  
  paretoSet = function(n = out.dim * 100L) {
    des = generateDesign(par.set = param.set, n = n)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    
    x1 = des[, 1L]
    x2 = des[, 2L]
    j = 3:in.dim
    
    tmp1 = sapply(seq_along(x1), function(i) 
      2 * x2[i] * sin(2 * pi * x1[i] + (j * pi) / in.dim))
    
    if (is.vector(tmp1))
      des[, -(1:2)] = tmp1
    else
      des[, -(1:2)] = t(tmp1)
    
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
    
    as.data.frame(des)
  }
  
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
