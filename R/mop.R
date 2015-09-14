#' MOP test function generator.
#' 
#' @param id [\code{integer(1)}] \cr
#'   Which MOP function? Valid values are 1, 2, ..., 7. \cr
#'   Which MOP_C function? Valid values are 1, 2, 3. 
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space. Specified  for each MOP function.
#' @param out.dim [\code{integer(1)}] \cr
#'   Size of target space. Specified  for each MOP function.
#' @return A \code{mooFunction}.
#' 
#' @references 
#' OHD. A. Van Veldhuizen, “Multiobjective Evolutionary Algorithms:
#' Classifications, Analyzes, and New Innovations,” Ph.D. Dissertation,
#' Air Force Institute of Technology, Wright-Patterson AFB, Jun. 1999.
#' 
#' @aliases mop MOP
#' 
#' @export
generateMOP = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id == 1 && in.dim != 1L)
    stop("MOP1 supports only in.dim = 1.")
  if (id %in% c(3, 5:7) && in.dim != 2L)
    stopf("MOP%i supports only in.dim = 2.", id)
  if (id == 4 && in.dim != 3L)
    stop("MOP4 supports only in.dim = 3.")
    
  if (id %in% c(1:4, 6) && out.dim != 2L)
    stopf("MOP%i supports only out.dim = 2.", id)
  if (id %in% c(5, 7) && out.dim != 3L)
    stopf("MOP%i supports only out.dim = 3.", id)
  
  assertChoice(id, 1:7)
  
  switch(id,
    generateMOP1(in.dim, out.dim), 
    generateMOP2(in.dim, out.dim), 
    generateMOP3(in.dim, out.dim), 
    generateMOP4(in.dim, out.dim), 
    generateMOP5(in.dim, out.dim),
    generateMOP6(in.dim, out.dim),
    generateMOP7(in.dim, out.dim)
  )
}

#' @rdname generateMOP
#' @export
generateMOP_C = function(id, in.dim = 30L, out.dim = 2L) {
  in.dim = asCount(in.dim)
  out.dim = asCount(out.dim)
  
  if (id %in% c(1, 3) && in.dim != 2L)
    stopf("MOP_C%i supports only in.dim = 2.", id)
  if (id == 2 && in.dim != 6L)
    stop("MOP_C2 supports only in.dim = 6.")
  
  if (id %in% c(1:2) && out.dim != 2L)
    stopf("MOP_C%i supports only out.dim = 2.", id)
  if (id == 3 && out.dim != 3L)
    stop("MOP_C3 supports only out.dim = 3.")
  
  assertChoice(id, 1:3)
  
  switch(id,
    generateMOP_C1(in.dim, out.dim), 
    generateMOP_C2(in.dim, out.dim), 
    generateMOP_C3(in.dim, out.dim)
  )
}