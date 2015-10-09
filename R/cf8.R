# CF8 test function generator.

generateCF8 = function(in.dim = 30L, out.dim = 2L) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = lz6(x)
      (f[1L]^2 + f[2L]^2) / (1 - f[3L]^2) - 
        3 * sin(2 * pi * ((f[1L]^2 - f[2L]^2) / (1 - f[3L]^2) + 1)) - 1 >= 0
    }))
  
  paretoSet = NULL
  
  paretoFront = function(n  = out.dim * 100L) {
    f3 = rep(runif(n / 5), each = 5)
    f1 = (rep(0:4, n / 5) / 4 * (1 - f3^2))^0.5
    f2 = (1 - f1^2 - f3^2)^0.5
    ## Was mit NaN?
    
    des = cbind(f1, f2, f3)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    des
    
  }
  
  mooFunction(
    name = "cf8",
    id = sprintf("cf8-%id-%id", in.dim, out.dim),
    fun = lz6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront)
}


