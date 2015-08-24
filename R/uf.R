#' UF test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which UF function? Valid values are 1, 2, ..., 10
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space.
#' @return A \code{mooFunction}.
#' 
#' @references
#' Zhang, Qingfu ; Zhou, Aimin ; Zhaoy, Shizheng ; Suganthany, Ponnuthurai N. ; 
#' Liu, Wudong ; Tiwari, Santosh: Multiobjective optimization Test Instances 
#' for the CEC 2009 Special Session and Competition / University of Essex and 
#' Nanyang Technological University. 2008. - Technical Report CES-487
#' 
#' @export
#' 
generateUF = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id %in% c(8:10) && out.dim != 3L)
    stop("This UF support only out.dim = 3.")
  if (id %in% c(1:7) && out.dim != 2L)
    stop("This UF support only out.dim = 2.")
  
  if (out.dim < 2L)
    stopf("You set your out.dim to %i. This is not multicrit! Set it at least to 2.", out.dim)
  
  if (in.dim < out.dim)
    stopf("YOu set out.dim = %i and in.dim = %i, but in.dim must be greatar than out.dim!.",
      out.dim, in.dim)
  
  assertChoice(id, 1:10)
  
  param.set = makeNumericParamSet(id = "x", len = in.dim, lower = 0, upper = 1)
  
  fun = switch(id,
    lz2,
    lz5,
    lz8,
    uf4,
    uf5,
    uf6,
    uf7,
    lz6,
    uf9,
    uf10
    )
  
  mooFunction(
    name = sprintf("uf%i", id),
    id = sprintf("uf%i-%id-$id", id, in.dim, out.dim),
    # Note: fun.args is a list here
    fun = function(x) fun(x, out.dim = out.dim),
    in.dim = in.dim,
    out.dim = out.dim,
    param.set = param.set,
    pareto.set = NULL,
    pareto.front = NULL)
}


# definition of uf4-10
uf4 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + 2 / length(j1) * 
    sum(abs(y(j1)) / (1 + exp(2 * abs(y(j1)))))
  f2 = 1 - x[1L]^2 + 2 / length(j2) * 
    sum(abs(y(j2)) / (1 + exp(2 * abs(y(j2)))))
  return(c(f1, f2))
}

uf5 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + 0.15 * abs(sin(20 * pi * x[1L])) + 2 / length(j1) * 
    sum(2 * y(j1)^2 - cos(4 * pi * y(j1)) + 1)
  f2 = 1 - x[1L] + 0.15 * abs(sin(20 * pi * x[1L])) + 2 / length(j2) * 
    sum(2 * y(j2)^2 - cos(4 * pi * y(j2)) + 1)
  return(c(f1, f2))
}

uf6 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L] + max(0, 0.7 * sin(4 * pi * x[1L])) + 2 / length(j1) * 
    (4 * sum(y(j1)^2) - 2 * prod(cos((20 * y(j1) * pi) / sqrt(j1))) + 2)
  f2 = 1 - x[1L] + max(0, 0.7 * sin(4 * pi * x[1L])) + 2 / length(j2) * 
    (4 * sum(y(j2)^2) - 2 * prod(cos((20 * y(j2) * pi) / sqrt(j2))) + 2)
  return(c(f1, f2))
}

uf7 = function(x, out.dim) {
  j = 2:length(x)
  j1 = j[j %% 2 == 1L]
  j2 = j[j %% 2 == 0L]
  
  y = function(j) {
    x[j] - sin(6 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = x[1L]^0.2 + 2 / length(j1) * sum(y(j1)^2)
  f2 = 1 - x[1L]^0.2 + 2 / length(j2) * sum(y(j2)^2)
  return(c(f1, f2))
}

uf9 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[j %% 3 == 1L]
  j2 = j[j %% 3 == 2L]
  j3 = j[j %% 3 == 0L]
  
  f1 = 0.5 * (max(0, 1.1 * (1 - 4 * (2 * x[1L] - 1)^2)) + 2 * x[1L]) * x[2L] + 
    2 / length(j1) * sum((x[j1] - 2 * x[2] * sin(2 * pi * x[1] + (j1 * pi) / length(x)))^2)
  f2 = 0.5 * (max(0, 1.1 * (1 - 4 * (2 * x[1L] - 1)^2)) - 2 * x[1L] + 2) * x[2L] +
    2 / length(j2) * 
    sum((x[j2] - 2 * x[2L] * sin(2 * pi * x[1L] + (j2 * pi) / length(x)))^2)
  f3 = 1 - x[2] + 2 / length(j3) * 
    sum((x[j3] - 2 * x[2L] * sin(2 * pi * x[1L] + (j3 * pi) / length(x)))^2)
  
  return(c(f1, f2, f3))
}

uf10 = function(x, out.dim) {
  j = 3:length(x)
  j1 = j[j %% 3 == 1L]
  j2 = j[j %% 3 == 2L]
  j3 = j[j %% 3 == 0L]
  
  y = function(j) {
    x[j] - 2 * x[2L] * sin(2 * pi * x[1L] + (j * pi) / length(x))
  }
  
  f1 = cos(0.5 * x[1L] * pi) * cos(0.5 * x[2L] * pi) + 
    2 / length(j1) * sum(4 * y(j1)^2 - cos(8 * pi * y(j1)) + 1)
  f2 = cos(0.5 * x[1L] * pi) * sin(0.5 * x[2L] * pi) + 
    2 / length(j2) * sum(4 * y(j2)^2 - cos(8 * pi * y(j2)) + 1)
  f3 = sin(0.5 * x[1L] * pi) + 
    2 / length(j3) * sum(4 * y(j3)^2 - cos(8 * pi * y(j3)) + 1)
  
  return(c(f1, f2, f3))
}