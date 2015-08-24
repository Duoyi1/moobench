context("uf")

test_that("uf1", {
  f = generateUF(1, in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(3, 0.03589838))
})

test_that("uf2", {
  f = generateUF(2, in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(8.22, 0.7449043))
})

test_that("uf3", {
  f = generateUF(3, in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1, 0))
})

test_that("uf4", {
  f = generateUF(4, in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1.238406, 0.116132), tolerance = 1e-6)
})

test_that("uf5", {
  f = generateUF(5, in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(5, 2.296875), tolerance = 1e-7)
})

test_that("uf6", {
  f = generateUF(6, in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(12.411474, 0.3605284), tolerance = 1e-5)
})

test_that("uf7", {
  f = generateUF(7, in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(3, 0.03589839))
})

test_that("uf8", {
  f = generateUF(8, in.dim = 5)
  value = f(c(1, 1, 1, 1, 1))
  expect_equal(value, c(0.06165, 2, 2.627616), tolerance = 1e-7)
})

test_that("uf9", {
  f = generateUF(9, in.dim = 5)
  value = f(c(1, 1, 1, 1, 1))
  expect_equal(value, c(1.06165, 2, 1.627616), tolerance = 1e-7)
})

test_that("uf10", {
  f = generateUF(10, in.dim = 5)
  value = f(c(1, 1, 1, 1, 1))
  expect_equal(value, c(2.837298, 8, 11.06382), tolerance = 1e-6)
})