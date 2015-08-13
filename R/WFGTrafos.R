ident = function(y) {
  y
}

bPoly = function(y, alpha) {
  y^alpha
}


bFlat = function(y, A, B, C) {
  tmp1 = pmin(0, floor(y - B)) * A * (B - y) / B
  tmp2 = pmin(0, floor(C - y)) * (1 - A) * (y - C) / (1 - C)
  A + tmp1 - tmp2
}

bParam = function(y, u, A, B, C) {
  v = A - (1 - 2 * u) * abs(floor(0.5 - u) + A)
  y^(B + (C - B) * v)
}

sLinear = function(y, A) {
  abs(y - A) / abs(floor(A - y) + A)
}

sDecept = function(y, A, B, C) {
  tmp1 = floor(y - A + B) * (1 - C + (A - B) / B) / (A - B) 
  tmp2 = floor(A + B - y) * (1 - C + (1 - A - B) / B) / (1 - A - B)
  1 + (abs(y - A) - B) * (tmp1 + tmp2 + 1 / B)
}

sMulti = function(y, A, B, C) {
  tmp1 = abs(y - C) / (2 * (floor(C - y) + C))
  tmp2 = (4 * A + 2) * pi * (0.5 - tmp1)
  (1 + cos(tmp2) + 4 * B * (tmp1)^2) / (B + 2)
}

## For these 2 functions y is a vector, not a single value
rSum = function(y, w) {
  sum(w * y) / sum(w)
}

rNonsep = function(y, A) {
  n = length(y)
  if (A == 1L) return(rSum(y, rep(1, n)))
  mat = array(dim = c(n, A - 1))
  for (i in 1:(A - 1))
    mat[, i] = y[1 + (i:(i + n - 1) %% n)]
  (sum(y) + sum(abs(y - mat))) / (n / A * ceiling(A / 2) * (1 + 2 * A - 2 * ceiling(A / 2)))
}