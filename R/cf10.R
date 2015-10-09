# CF10 test function generator.

generateCF10= function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = uf10(x)
      (f[1L]^2 + f[2L]^2) / (1 - f[3L]^2) - 
        sin(2 * pi * ((f[1L]^2 - f[2L]^2) / (1 - f[3L]^2) + 1)) - 1 >= 0
    }))
  
  paretoSet = NULL
  
  paretoFront = function(n = out.dim * 100L) {
    n = floor(n / 3)
    
    f1 = rep(0, n)
    f2 = runif(n)
    f3 = (1 - f2^2)^0.5
    
    
    ff3 = runif(n)
    ff1 = runif(n, min = (0.25 * (1 - ff3^2))^0.5, max = (0.5 * (1 - ff3^2))^0.5)
    ff2 = (1 - ff1^2 - ff3^2)^0.5
    
    fff3 = runif(n)
    fff1 = runif(n, min = (0.75 * (1 - fff3^2))^0.5, max = (1 - fff3^2)^0.5)
    fff2 = (1 - fff1^2 - fff3^2)^0.5
    
    des = cbind(c(f1, ff1, fff1), c(f2, ff2, fff2), c(f3, ff3, fff3))
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    des
    
  }
  
  mooFunction(
    name = "cf10",
    id = sprintf("cf10-%id-%id", in.dim, out.dim),
    fun = uf10,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}

