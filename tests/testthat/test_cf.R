context("cf")

test_that("cf1", {
  e = generateCF(1, in.dim = 3L)
  value = e(c(1, 1, 1))
  expect_equal(value, c(1, 0))
  f = generateCF(1, in.dim = 3L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 10)))
  g = generateCF(1, in.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 10))
  expect_equal(value, c(NA_real_, NA_real_))
})

test_that("cf2", {
  f = generateCF(2, in.dim = 3L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1)))
  g = generateCF(2, in.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 1))
  expect_equal(value, c(NA_real_, NA_real_))
})

test_that("cf3", {
  f = generateCF(3, in.dim = 3L, on.infeasible = "NA")
  expect_error(f(c(1, 1, 1)))
  g = generateCF(3, in.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 1))
  expect_equal(value, c(NA_real_, NA_real_))
})

test_that("cf4", {
  f = generateCF(4, in.dim = 3L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1)))
  g = generateCF(4, in.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 1))
  expect_equal(value, c(NA_real_, NA_real_))
})

test_that("cf5", {
  f = generateCF(5, in.dim = 3L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1)))
  g = generateCF(5, in.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 1))
  expect_equal(value, c(NA_real_, NA_real_))
})

test_that("cf6", {
  f = generateCF(6, in.dim = 4L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1, 1)))
  g = generateCF(6, in.dim = 4L, on.infeasible = "NA")
  value = g(c(1, 1, 1, 1))
  expect_equal(value, c(NA_real_, NA_real_))
})

test_that("cf7", {
  e = generateCF(7, in.dim = 5L)
  value = e(c(1, 1, 1, 1, 1))
  expect_equal(value, c(14.1644199, 0.1723165))
  f = generateCF(7, in.dim = 5L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1, 1, 10)))
  g = generateCF(7, in.dim = 5L, on.infeasible = "NA")
  value = g(c(1, 1, 1, 1, 10))
  expect_equal(value, c(NA_real_, NA_real_))
})

test_that("cf8", {
  f = generateCF(8, in.dim = 5L, out.dim = 3L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1, 1, 1)))
  g = generateCF(8, in.dim = 5L, out.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 1, 1, 1))
  expect_equal(value, c(NA_real_, NA_real_, NA_real_))
})

test_that("cf9", {
  f = generateCF(9, in.dim = 5L, out.dim = 3L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1, 1, 1)))
  g = generateCF(9, in.dim = 5L, out.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 1, 1, 1))
  expect_equal(value, c(NA_real_, NA_real_, NA_real_))
})

test_that("cf10", {
  e = generateCF(10, in.dim = 5L, out.dim = 3L)
  value = e(c(1, 1, 1, 1, 1))
  expect_equal(value, c(2.837298, 8, 11.06382), tolerance = 1e-6)
  f = generateCF(10, in.dim = 5L, out.dim = 3L, on.infeasible = "stop")
  expect_error(f(c(1, 1, 1, 1, 10)))
  g = generateCF(9, in.dim = 5L, out.dim = 3L, on.infeasible = "NA")
  value = g(c(1, 1, 1, 1, 10))
  expect_equal(value, c(NA_real_, NA_real_, NA_real_))
})