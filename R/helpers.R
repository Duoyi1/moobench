
# assert input and eval fun
evalMooFunction = function(fun, x, in.dim, lower.bounds, upper.bounds) {
  assertNumeric(x, len = in.dim)
  if (any(x < lower.bounds | x > upper.bounds))
    stop("Input out of bounds.")
  
  fun(x)
}