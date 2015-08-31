


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
    lz1
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
          x = x[[1]]
        f = lz1(x)
        f[1] + f[2] - abs(sin(10 * pi * (f[1] - f[2] + 1))) - 1 >= 0
      }))
  )
}

