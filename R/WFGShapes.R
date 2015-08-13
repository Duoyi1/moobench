linear = function(x, alpha, beta, A) {
  len = length(x)
  
  f1 = prod(x)
  fm = 1 - x[1L]
  
  if (len > 2L) {
    f = rev(cumprod(x[-len]) * (1 - x[-1L]))
    return(c(f1, f, fm))
  }
  return(c(f1, fm))
}

convex = function(x, alpha, beta, A) {
  len = length(x)
  
  f1 = prod(1 - cos(x * pi / 2))
  fm = 1 - sin(x[1L] * pi / 2)
  
  if (len > 2L) {
    f = rev(cumprod(1 - cos(x[-len] * pi / 2)) * (1 - sin(x[-1L] * pi / 2)))
    return(c(f1, f, fm))
  }
  return(c(f1, fm))
}

concave = function(x, alpha, beta, A) {
  len = length(x)
  
  f1 = prod(sin(x * pi / 2))
  fm = cos(x[1L] * pi / 2)
  
  if (len > 2L) {
    f = rev(cumprod(sin(x[-len] * pi / 2)) * (cos(x[-1L] * pi / 2)))
    return(c(f1, f, fm))
  }
  return(c(f1, fm))
}

mixed = function(x, alpha, beta, A) {
  tmp = 2 * A * pi
  return(c((1 - x[1L] - cos(tmp * x[1L] + pi / 2) / tmp)^alpha))
}

disconnected = function(x, alpha, beta, A) {
  return(c(1 - x[1L]^alpha * cos(A * x[1L]^beta * pi)^2))
}