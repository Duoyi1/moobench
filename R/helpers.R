
# assert input and eval fun
evalMooFunction = function(fun, x, in.dim, out.dim, lower.bounds, upper.bounds) {
  assertNumeric(x, len = in.dim)
  # FIXME: Return NA if constraints are not satisfied? And so we can support
  # test functions with constraints? Think about it ...
  if (any(x < lower.bounds | x > upper.bounds))
    stop("Input out of bounds.")
  
  fun(x, out.dim)
}