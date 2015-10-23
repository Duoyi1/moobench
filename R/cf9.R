# CF9 test function generator.

generateCF9 = function(in.dim = 30L, out.dim = 2L, on.infeasible) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim,
      lower = c(0, 0, rep(-2, in.dim - 2)), upper = c(1, 1, rep(2, in.dim - 2))),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = lz6(x)
      (f[1L]^2 + f[2L]^2) / (1 - f[3L]^2) - 
        3 * sin(2 * pi * ((f[1L]^2 - f[2L]^2) / (1 - f[3L]^2) + 1)) - 1 >= 0
    }))
  
  paretoSet = NULL
  
  paretoFront = function(n = out.dim * 100L) {
    N = floor(n / 3)
    
    if (N * 3 == n) {
      f1 = rep(0, N)
      f2 = runif(N)
      f3 = (1 - f2^2)^0.5
      
      ff3 = runif(N)
      ff1 = runif(N, min = (0.25 * (1 - ff3^2))^0.5, max = (0.5 * (1 - ff3^2))^0.5)
      ff2 = (1 - ff1^2 - ff3^2)^0.5
      
      fff3 = runif(N)
      fff1 = runif(N, min = (0.75 * (1 - fff3^2))^0.5, max = (1 - fff3^2)^0.5)
      fff2 = (1 - fff1^2 - fff3^2)^0.5
    } else {
      f1 = rep(0, n - N * 2)
      f2 = runif(n - N * 2)
      f3 = (1 - f2^2)^0.5
      
      ff3 = runif(N)
      ff1 = runif(N, min = (0.25 * (1 - ff3^2))^0.5, max = (0.5 * (1 - ff3^2))^0.5)
      ff2 = (1 - ff1^2 - ff3^2)^0.5
      
      fff3 = runif(N)
      fff1 = runif(N, min = (0.75 * (1 - fff3^2))^0.5, max = (1 - fff3^2)^0.5)
      fff2 = (1 - fff1^2 - fff3^2)^0.5
    }
    
    des = cbind(c(f1, ff1, fff1), c(f2, ff2, fff2), c(f3, ff3, fff3))
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    des
  }
  
  mooFunction(
    name = "cf9",
    id = sprintf("cf9-%id-%id", in.dim, out.dim),
    fun = lz6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront,
    on.infeasible = on.infeasible)
}

