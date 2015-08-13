makeWFGTrafo = function(x, lists){
  len = length(lists)
  res = list()
  for (i in 1:len) {
    res[[i]] = do.call(lists[[i]]$name, list(y = lists[[i]]$ids, 
      alpha = lists[[i]]$params$alpha,
      beta = lists[[i]]$params$beta,
      A = lists[[i]]$params$A,
      B = lists[[i]]$params$B,
      C = lists[[i]]$params$C,
      w = lists[[i]]$params$w
      ))
  }
  
  return(res)
}