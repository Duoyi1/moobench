# CF6 test function generator.

generateCF6 = function(in.dim = 30L, out.dim = 2L, on.infeasible) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      const1 = x[2L] - 0.8 * x[1L] * sin(6 * pi * x[1L] + 0.2 * pi) - sign(0.5 * (1 - x[1L]) - 
          (1 - x[1L])^2) * sqrt(abs(0.5 * (1 - x[1L]) - (1 - x[1L])^2)) >= 0 
      const2 = x[4L] - 0.8 * x[1L] * sin(6 * pi * x[1L] + 0.4 * pi) - sign(0.25 * sqrt(1 - x[1L]) - 
          0.5 * (1 - x[1L])) * sqrt(abs(0.25 * sqrt(1 - x[1L]) - 0.5 * (1 - x[1L]))) >= 0
      const1 && const2
    }))
  
  paretoSet = NULL
  
  paretoFront = function(n = out.dim * 100L) {
    pts1 = runif(n)
    pts2 = numeric(n)
    
    pts2[0 <= pts1 & pts1 <= 0.5] = (1 -  pts1[0 <= pts1 & pts1 <= 0.5])^2
    pts2[0.5 < pts1 & pts1 <= 0.75] = 0.5 * (1 - pts1[0.5 < pts1 & pts1 <= 0.75])
    pts2[0.75 < pts1 & pts1 <= 1] = 0.25 * sqrt(1 -  pts1[0.75 < pts1 & pts1 <= 1])
    
    des = cbind(pts1, pts2)
    des = des[order(des[, 1L]), ]
    rownames(des) = 1:nrow(des)
    des
    
  }
  
  mooFunction(
    name = "cf6",
    id = sprintf("cf6-%id-%id", in.dim, out.dim),
    fun = cf6,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront,
    on.infeasible = on.infeasible)
}


# Definition of cf6
cf6 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y1 = x[j1] - 0.8 * x[1L] * cos(6 * pi * x[1L] + (j1 * pi) / length(x))
  y2 = x[j2] - 0.8 * x[1L] * sin(6 * pi * x[1L] + (j2 * pi) / length(x))
  
  f1 = x[1L] + sum(y1^2)
  f2 = (1 - x[1L])^2 + sum(y2^2)
  return(c(f1, f2))
}
