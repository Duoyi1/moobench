#' LZ test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which LZ function? Valid values are 1, 2, ..., 9
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' Li, Hui ; Zhang, Qingfu: Multiobjective Optimization Problems With
#' Complicated Pareto Sets, MOEA/D and NSGA-II. In: IEEE Trans.
#' Evolutionary Computation 13 (2009), No. 2, pp. 284-302
#' 
#' @export
#'
#'  
generateLZ = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id == 6 && out.dim != 3L)
      stop("LZ6 support only out.dim = 3.")
  if (id != 6 && out.dim != 2L)
      stop("LZ support only out.dim = 2.")

  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("YOu set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:9)
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  fun = switch(id,
    lz1, 
    lz2,
    lz3,
    lz4,
    lz5,
    lz6,
    lz7,
    lz8,
    lz9
  )
  
  mooFunction(
    name = sprintf("lz%i", id),
    id = sprintf("lz%i-%id-$id", id, in.dim, out.dim),
    # Note: fun.args is a list here
    fun = function(x) fun(x, out.dim = out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    pareto.set = NULL,
    pareto.front = NULL)
}


# definition of lz1-9
lz1 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - x[1L]^(0.5 * (1 + (3 * (j1 - 2)) / (length(x) - 2))))^2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum((x[j2] - x[1L]^(0.5 * (1 + (3 * (j2 - 2)) / (length(x) - 2))))^2)
  return(c(f1, f2))
}

lz2 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - sin(6 * pi * x[1L] + (j1 * pi) / length(x)))^2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum((x[j2] - sin(6 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  return(c(f1, f2))
}

lz3 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - 0.8 * x[1L] * cos(6 * pi * x[1L] + (j1 * pi) / length(x)))^2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum((x[j2] - 0.8 * x[1L] * sin(6 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  return(c(f1, f2))
}

lz4 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - 0.8 * x[1L] * cos((6 * pi * x[1L] + (j1 * pi) / length(x)) / 3))^2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum((x[j2] - 0.8 * x[1L] * sin(6 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  return(c(f1, f2))
}

lz5 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - (0.3 * x[1L]^2 * cos(24 * pi * x[1L] + (4 * j1 * pi) / length(x)) + 0.6 * x[1L]) * 
        cos(6 * pi * x[1L] + (j1 * pi) / length(x) ))^2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum((x[j2] - (0.3 * x[1L]^2 * cos(24 * pi * x[1L] + (4 * j2 * pi) / length(x)) + 0.6 * x[1L]) * 
        sin(6 * pi * x[1L] + (j2 * pi) / length(x) ))^2)
  return(c(f1, f2))
}

lz6 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[j %% 3 == 1L]
  j2 = j[j %% 3 == 2L]
  j3 = j[j %% 3 == 0L]
  
  f1 = cos(0.5 * x[1L] * pi) * cos(0.5 * x[2L] * pi) + 2 / length(j1) * 
    sum((x[j1] - 2 * x[2L] * sin(2 * pi * x[1L] + (j1 * pi) / length(x)))^2)
  f2 = cos(0.5 * x[1L] * pi) * sin(0.5 * x[2L] * pi) + 2 / length(j2) * 
    sum((x[j2] - 2 * x[2L] * sin(2 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  f3 = sin(0.5 * x[1L] * pi) + 2 / length(j3) * 
    sum((x[j3] - 2 * x[2L] * sin(2 * pi * x[1L] + (j3 * pi) / length(x)))^2)
  
  return(c(f1, f2, f3))
}

lz7 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - x[1L]^(0.5 * (1 + (3 * (j - length(x))) / (length(x) - 2)))
  }
  
  f1 = x[1L] + 2 / length(j1) * 
    sum(y(j1)^2 - cos(8 * y(j1) * pi) + 1)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    sum(4 * y(j2)^2 - cos(8 * y(j2) * pi) + 1)
  return(c(f1, f2))
}

lz8 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - x[1L]^(0.5 * (1 + (3 * (j - length(x))) / (length(x) - 2)))
  }
  
  f1 = x[1L] + 2 / length(j1) * 
    (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / (sqrt(j1)))) + 2)
  f2 = 1 - sqrt(x[1L]) + 2 / length(j2) * 
    (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / (sqrt(j2)))) + 2)
  return(c(f1, f2))
}

lz9 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  f1 = x[1L] + 2 / length(j1) * 
    sum((x[j1] - sin(6 * pi * x[1L] + (j1 * pi) / length(x)))^2)
  f2 = 1 - x[1L]^2 + 2 / length(j2) * 
    sum((x[j2] - sin(6 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  return(c(f1, f2))
}