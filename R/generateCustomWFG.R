

generateCustomWFG = function(z.max, S, D, A, trafos, shapeTrafos) {
  assertNumber(D, lower = 0)
  assertNumeric(A, lower = 0, upper = 1)
  assertNumeric(S, lower = 0)
  
  
  function(z) {
    z = z / z.max
    for (i in seq_along(trafos))
      z = trafos[[i]](z)
    z = c(max(z[out.dim], A) * (z[-out.dim] - 0.5), z[out.dim])
    sapply(seq_along(shapeTrafo), function(i) D * z[out.dim] + S[i] * shapeTrafo[[i]](z[-out.dim]))
  }
}