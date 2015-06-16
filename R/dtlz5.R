dtlz5_wir = function(x, in.dim, out.dim) {
  x.head = x[seq_len(out.dim - 1)]
  x.tail = x[out.dim:length(x)] - 0.5
  
  g = sum(x.tail)^0.1
  
  theta = numeric(out.dim - 1)
  theta[1] = x.head[1] * 0.5 * pi
  theta[2:(out.dim - 1)] = (pi / (4 * (1 + g))) * (1 + 2 * g * x.head[2:(out.dim - 1)]) * 0.5 * pi

  rev((1 + g) * c(sin(theta), 1) * c(1, cumprod(cos(theta))))
}
