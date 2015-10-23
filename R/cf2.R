# CF2 test function generator.

generateCF2 = function(in.dim = 30L, out.dim = 2L, on.infeasible) {
  
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim,
      lower = c(0, rep(-1, in.dim - 1)), upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      f = lz2(x)
      tt = f[2L] + sqrt(f[1L]) - sin(2 * pi * (sqrt(f[1L]) - f[2L] + 1)) - 1
      tt / (1 + exp(4 * abs(tt))) >= 0
    }))
  
  paretoSet = NULL
  
  paretoFront = function(n = out.dim * 100L) {
    if (n %% 2 == 1) {
      pts1 = runif(n = floor(n / 2), min = 1 / 16, max = 4 / 16)
      pts2 = runif(n = floor(n / 2), min = 9 / 16, max = 1)
    } else {
      pts1 = runif(n = n / 2, min = 1 / 16, max = 4 / 16)
      pts2 = runif(n = n / 2 - 1, min = 9 / 16, max = 1)
    }

    des = cbind(c(0, pts1, pts2), 1 - sqrt(c(0, pts1, pts2)))
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    des
    
  }
  
  mooFunction(
    name = "cf2",
    id = sprintf("cf2-%id-%id", in.dim, out.dim),
    fun = lz2,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront,
    on.infeasible = on.infeasible)
}
