
# assert input and eval fun
# Note: fun.args is a list here!
evalMooFunction = function(fun, param.set, out.dim, on.infeasible, ...) {
  if (!isFeasible(param.set, list(...)))
    switch(on.infeasible,
      `stop` = stop("Input not feasible."),
      `NA` = rep(NA_real_, out.dim)
    )
  else
    fun(...)
}