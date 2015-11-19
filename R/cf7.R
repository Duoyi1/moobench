# CF7 test function generator.

generateCF7 = function(in.dim = 30L, out.dim = 2L, on.infeasible) {
  
  param.set = makeParamSet(
    makeNumericVectorParam(id = "x", len = in.dim,
      lower = c(0, rep(-2, in.dim - 1)), upper = c(1, rep(2, in.dim - 1))),
    forbidden = expression({
      #FIXME: Ask someone intelligent!! This is not good!!!
      if (is.list(x))
        x = x[[1L]]
      const1 = x[2L] - sin(6 * pi * x[1L] + 0.2 * pi) - sign(0.5 * (1 - x[1L]) - 
          (1 - x[1L])^2) * sqrt(abs(0.5 * (1 - x[1L]) - (1 - x[1L])^2)) >= 0 
      const2 = x[4L] - sin(6 * pi * x[1L] + 0.4 * pi) - sign(0.25 * sqrt(1 - x[1L]) - 
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
    as.data.frame(des)
    
  }
  
  mooFunction(
    name = "cf7",
    id = sprintf("cf7-%id-%id", in.dim, out.dim),
    fun = cf7,
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    paretoSet = paretoSet,
    paretoFront = paretoFront,
    on.infeasible = on.infeasible)
}

# Definition of cf7
cf7 = function(x) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y1 = x[j1] - cos(6 * pi * x[1L] + (j1 * pi) / length(x))
  y2 = x[j2] - sin(6 * pi * x[1L] + (j2 * pi) / length(x))
  
  f1 = x[1L] + sum(2 * y1^2 - cos(4 * pi * y1) + 1)
  f2 = (1 - x[1L])^2 + sum(c(y2[1:2]^2, 2 * y2[-(1:2)]^2 - cos(4 * pi * y2[-(1:2)]) + 1))
  return(c(f1, f2))
}
