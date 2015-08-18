

generateCustomWFG = function(z.max, S, D, A, trafos, shapeTrafos) {
  assertNumber(D, lower = 0)
  assertNumeric(A, lower = 0, upper = 1)
  assertNumeric(S, lower = 0)
  
  
  custumWFG = function(z) {
    #z = z / z.max
    for (i in seq_along(trafos))
      z = trafos[[i]](z)
    z = c(max(z[out.dim], A) * (z[-out.dim] - 0.5) + 0.5, z[out.dim])
    sapply(seq_along(shapeTrafos), function(i) D * z[out.dim] + S[i] * shapeTrafos[[i]](z[-out.dim]))
  }
  
  custumWFG = addClasses(custumWFG, "custumWFGFun")
  return(custumWFG)
}