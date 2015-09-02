


generateCF = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id %in% c(8:10) && out.dim != 3L)
    stop("This UF support only out.dim = 3.")
  if (id %in% c(1:7) && out.dim != 2L)
    stop("This UF support only out.dim = 2.")
  
  if (in.dim < out.dim)
    stopf("You set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:10)
  
  if (id == 1L) {
    
  }
  param.set = makeCFParamset(id, in.dim)
  
  fun = switch(id,
    lz1,
    lz2,
    cf3,
    cf4,
    cf5,
    cf6
  )
  
  mooFunction(
    name = sprintf("cf%i", id),
    id = sprintf("cf%i-%id-$id", id, in.dim, out.dim),
    # Note: fun.args is a list here
    fun = function(x) fun(x, out.dim = out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    pareto.set = NULL,
    pareto.front = NULL)
}

makeCFParamset = function(id, in.dim) {
  switch(id,
    makeParamSet(
      makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
      forbidden = expression({
        #FIXME: Ask someone intelligent!! This is not good!!!
        if (is.list(x))
          x = x[[1L]]
        f = lz1(x)
        f[1L] + f[2L] - abs(sin(10 * pi * (f[1L] - f[2L] + 1))) - 1 >= 0
      })),
    makeParamSet(
      makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
      forbidden = expression({
        #FIXME: Ask someone intelligent!! This is not good!!!
        if (is.list(x))
          x = x[[1L]]
        f = lz2(x)
        tt = f[2L] + sqrt(f[1L]) - sin(2 * pi * (sqrt(f[1L]) - f[2L] + 1)) - 1
        tt / (1 + exp(4 * abs(tt))) >= 0
      })),
    makeParamSet(
      makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
      forbidden = expression({
        #FIXME: Ask someone intelligent!! This is not good!!!
        if (is.list(x))
          x = x[[1L]]
        f = cf3(x)
        f[2L] + f[1L]^2 - sin(2 * pi * (f[1L]^2 - f[2L] + 1)) - 1 >= 0
      })),
    makeParamSet(
      makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
      forbidden = expression({
        #FIXME: Ask someone intelligent!! This is not good!!!
        if (is.list(x))
          x = x[[1L]]
        tt = x[2L] - sin(6 * pi * x[1L] + 0.2 * pi) - 0.5 * x[1L] + 0.25
        tt / (1 + exp(4 * abs(tt))) >= 0
      })),
    makeParamSet(
      makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
      forbidden = expression({
        #FIXME: Ask someone intelligent!! This is not good!!!
        if (is.list(x))
          x = x[[1L]]
        x[2L] - 0.8 * x[1L] * sin(6 * pi * x[1L] + 0.2 * pi) - 0.5 * x[1L] + 0.25 >= 0
      })),
    makeParamSet(
      makeNumericVectorParam(id = "x", len = in.dim, lower = 0, upper = 1),
      forbidden = expression({
        #FIXME: Ask someone intelligent!! This is not good!!!
        if (is.list(x))
          x = x[[1L]]
        x[2L] - 0.8 * x[1L] * sin(6 * pi * x[1L] + 0.2 * pi) - sign(0.5 * (1 - x[1L]) - 
            (1 - x[1L])^2) * sqrt(abs(0.5 * (1 - x[1L]) - (1 - x[1L])^2)) >= 0 &
        x[4L] - 0.8 * x[1L] * sin(6 * pi * x[1L] + 0.4 * pi) - sign(0.25 * sqrt(1 - x[1L]) - 
            0.5 * (1 - x[1L])) * sqrt(abs(0.25 * sqrt(1 - x[1L]) - 0.5 * (1 - x[1L]))) >= 0
      }))
  )
}

# definition of cf3-5
cf3 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - x[1L]^(0.5 * (1 + (3 * (j - length(x))) / (length(x) - 2)))
  }
  
  f1 = x[1L] + 2 / length(j1) * 
    (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / (sqrt(j1)))) + 2)
  f2 = 1 - x[1L]^2 + 2 / length(j2) * 
    (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / (sqrt(j2)))) + 2)
  return(c(f1, f2))
}

cf4 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + sum(y(j1)^2)
  if (y(j2)[1L] < ((3 / 2) * (1 - sqrt(2) / 2))) {
    f2 = 1 - x[1L] + sum(c(abs(y(j2)[1L]), y(j2)[-1L]^2))
  }
  else
    f2 = 1 - x[1L] + sum(c(0.125 + (y(j2)[1L] - 1)^2, y(j2)[-1L]^2))
  return(c(f1, f2))
}

cf5 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y1 = x[j1] - 0.8 * cos(6 * pi * x[1L] + (j1 * pi) / length(x))
  y2 = x[j2] - 0.8 * sin(6 * pi * x[1L] + (j2 * pi) / length(x))
  
  f1 = x[1L] + sum(2 * y1^2 - cos(4 * pi * y1) + 1)
  if (y2[1] < ((3 / 2) * (1 - sqrt(2) / 2))) {
    f2 = 1 - x[1L] + sum(c(abs(y2[1L]), 2 * y2[-1L]^2 - cos(4 * pi * y2[-2L]) + 1))
  }
  else
    f2 = 1 - x[1L] + sum(c(0.125 + (y2[1L] - 1)^2, 2 * y2[-1L]^2 - cos(4 * pi * y2[-2L]) + 1))
  return(c(f1, f2))
}