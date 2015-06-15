dtlz4_wir = function(x, in.dim, out.dim) {
  x.head = x[seq_len(out.dim - 1)]^100 * 0.5 * pi
  x.tail = x[out.dim:length(x)] - 0.5
  
  rev((1 + sum(x.tail^2)) * c(sin(x.head), 1) * c(1, cumprod(cos(x.head))))
}
