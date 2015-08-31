
# assert input and eval fun
# Note: fun.args is a list here!
evalMooFunction = function(fun, param.set, ...) {
  # FIXME: Return NA if constraints are not satisfied? And so we can support
  # test functions with constraints? Think about it ...
  if (!isFeasible(param.set, list(...)))
    stop("Input not feasible.")
  fun(...)
}