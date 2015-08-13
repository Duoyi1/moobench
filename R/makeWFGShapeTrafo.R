makeWFGShapeTrafo = function(x, lists){
  len = length(lists)
  res = list()
  for (i in 1:len) {
    res[[i]] = do.call(lists[[i]]$name, list(x = x, 
      alpha = lists[[i]]$params$alpha,
      beta = lists[[i]]$params$beta,
      A = lists[[i]]$params$A))
  }
  
  return(res)
}