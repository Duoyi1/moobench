dtlz5_wir = function(x, in.dim, out.dim) {
  x.head = x[seq_len(out.dim - 1)]
  x.tail = x[out.dim:length(x)] - 0.5
  
  theta = (pi / (4 * (1 + sum(x.tail^2)))) * (1 + 2 * sum(x.tail^2) * x.head) * 0.5 * pi
  #theta = ((0.5 * pi^2) / (4 * (1 + sum(x.tail^2)))) * (1 + 2 * sum(x.tail^2) * x.head)
  
  rev((1 + sum(x.tail^2)) * c(sin(theta), 1) * c(1, cumprod(cos(theta))))
}
