

generateCustomWFG = function(x.max, S, D, A, trafos, shapeTrafos) {
  assertNumber(D, lower = 0)
  assertNumeric(A, lower = 0, upper = 1)
  assertNumeric(S, lower = 0)
  out.dim = length(shapeTrafos)
  
  custumWFG = function(x) {
    x = x / x.max
    print(x)
    for (i in seq_along(trafos)) {
      x = trafos[[i]](x)
      print(x)
    }
    x = c(max(x[out.dim], A) * (x[-out.dim] - 0.5) + 0.5, x[out.dim])
    print(x)
    sapply(seq_along(shapeTrafos), function(i) D * x[out.dim] + S[i] * shapeTrafos[[i]](x[-out.dim]))
  print(x)
    }
  
  custumWFG = addClasses(custumWFG, "custumWFGFun")
  return(custumWFG)
}