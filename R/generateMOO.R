#' Genrate MOO functions
#' 
#' @param name [\code{character(1)}] \cr
#'   Which MOO function? (DTLZ, ZDT, LZ, UF, CF)
#' @param ...  [\code{any}] \cr
#'   Noch was schreiben
#' @return A \code{mooFunction}.
#' 
#' @export
#' 
generateMOO = function(name = "", ...) {
  
  fun = switch(name,
    zdt1 = generateZDT(id = 1, ...),
    zdt2 = generateZDT(id = 2, ...),
    zdt3 = generateZDT(id = 3, ...),
    zdt4 = generateZDT(id = 4, ...),
    zdt6 = generateZDT(id = 6, ...),
    dtlz1 = generateDTLZ(id = 1, ...), 
    dtlz2 = generateDTLZ(id = 2, ...), 
    dtlz3 = generateDTLZ(id = 3, ...), 
    dtlz4 = generateDTLZ(id = 4, ...), 
    dtlz5 = generateDTLZ(id = 5, ...), 
    dtlz6 = generateDTLZ(id = 6, ...),
    lz1 = generateLZ(id = 1, ...),  
    lz2 = generateLZ(id = 2, ...), 
    lz3 = generateLZ(id = 3, ...), 
    lz4 = generateLZ(id = 4, ...), 
    lz5 = generateLZ(id = 5, ...), 
    lz6 = generateLZ(id = 6, ...), 
    lz7 = generateLZ(id = 7, ...), 
    lz8 = generateLZ(id = 8, ...), 
    lz9 = generateLZ(id = 9, ...), 
    uf1 = generateUF(id = 1, ...), 
    uf2 = generateUF(id = 2, ...), 
    uf3 = generateUF(id = 3, ...), 
    uf4 = generateUF(id = 4, ...), 
    uf5 = generateUF(id = 5, ...), 
    uf6 = generateUF(id = 6, ...), 
    uf7 = generateUF(id = 7, ...), 
    uf8 = generateUF(id = 8, ...), 
    uf9 = generateUF(id = 9, ...), 
    uf10 = generateUF(id = 10, ...),
    cf1 = generateCF(id = 1, ...), 
    cf2 = generateCF(id = 2, ...), 
    cf3 = generateCF(id = 3, ...), 
    cf4 = generateCF(id = 4, ...), 
    cf5 = generateCF(id = 5, ...), 
    cf6 = generateCF(id = 6, ...), 
    cf7 = generateCF(id = 7, ...), 
    cf8 = generateCF(id = 8, ...), 
    cf9 = generateCF(id = 9, ...), 
    cf10 = generateCF(id = 10, ...),
    stop("Unknown ID")
  )
  
}