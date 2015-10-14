# CF4 test function generator.

generateCF4 = function(in.dim = 30L, out.dim = 2L, on.infeasible) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      tt = x[2L] - sin(6 * pi * x[1L] + 0.2 * pi) - 0.5 * x[1L] + 0.25
      tt / (1 + exp(4 * abs(tt))) >= 0
    }))
  
  paretoSet = NULL
  
  paretoFront = function(n = out.dim * 100L) {
    pts1 = runif(n)
    pts2 = numeric(n)
    
    pts2[0 <= pts1 & pts1 <= 0.5] = 1 -  pts1[0 <= pts1 & pts1 <= 0.5]
    pts2[0.5 < pts1 & pts1 <= 0.75] = -0.5 * pts1[0.5 < pts1 & pts1 <= 0.75] + 0.75
    pts2[0.75 < pts1 & pts1 <= 1] = 1 -  pts1[0.75 < pts1 & pts1 <= 1] + 0.125
    
    des = cbind(pts1, pts2)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    des
    
  }
  
  mooFunction(
    name = "cf4",
    id = sprintf("cf4-%id-%id", in.dim, out.dim),
    fun = cf4,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront,
    on.infeasible = on.infeasible)
}

# Definition of cf4
cf4 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + sum(y(j1)^2)
  if (y(j2)[1L] < ((3 / 2) * (1 - sqrt(2) / 2))) {
    f2 = 1 - x[1L] + sum(c(abs(y(j2)[1L]), y(j2)[-1L]^2))
  }
  else
    f2 = 1 - x[1L] + sum(c(0.125 + (y(j2)[1L] - 1)^2, y(j2)[-1L]^2))
  return(c(f1, f2))
}
