dtlz1_wir = function(x, in.dim, out.dim) {
  x.head = x[seq_len(out.dim - 1)]
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev(0.5 * c(1, cumprod(x.head)) * c(1 - x.head, 1) * 
      (1 + 100 * (length(x.tail) + sum(x.tail^2 - cos(20 * pi * x.tail)))))
}


