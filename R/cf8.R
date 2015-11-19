# CF8 test function generator.

generateCF8 = function(in.dim = 30L, out.dim = 2L, on.infeasible) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim,
      lower = c(0, 0, rep(-4, in.dim - 2)), upper = c(1, 1, rep(4, in.dim - 2))),
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
    # Due to bad numeric we have calc some roots of values like -1e-17.
    f2[is.na(f2)] = 0
    
    des = cbind(f1, f2, f3)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    as.data.frame(des)
    
  }
  
  mooFunction(
    name = "cf8",
    id = sprintf("cf8-%id-%id", in.dim, out.dim),
    fun = lz6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront,
    on.infeasible = on.infeasible)
}


