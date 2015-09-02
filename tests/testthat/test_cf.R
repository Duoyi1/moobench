context("cf")

test_that("cf1", {
  f = generateCF(1, in.dim = 3L)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1, 0))
})

test_that("cf2", {
  f = generateCF(2, in.dim = 3L)
  value = f(c(1, 1, 1))
  #expect_equal(value, c())
})

test_that("cf3", {
  f = generateCF(3, in.dim = 3L)
  value = f(c(1, 1, 1))
  #expect_equal(value, c())
})

test_that("cf4", {
  f = generateCF(4, in.dim = 3L)
  value = f(c(1, 1, 1))
  #expect_equal(value, c(), tolerance = 1e-6)
})

test_that("cf5", {
  f = generateCF(5, in.dim = 3L)
  value = f(c(1, 1, 1))
  #expect_equal(value, c(), tolerance = 1e-7)
})

test_that("cf6", {
  f = generateCF(6, in.dim = 3L)
  value = f(c(1, 1, 1))
  #expect_equal(value, c(), tolerance = 1e-5)
})

test_that("cf7", {
  f = generateCF(7, in.dim = 3L)
  value = f(c(1, 1, 1))
  #expect_equal(value, c())
})

test_that("cf8", {
  f = generateCF(8, in.dim = 5L, out.dim = 3L)
  value = f(c(1, 1, 1, 1, 1))
  #expect_equal(value, c(), tolerance = 1e-7)
})

test_that("cf9", {
  f = generateCF(9, in.dim = 5L, out.dim = 3L)
  value = f(c(1, 1, 1, 1, 1))
  #expect_equal(value, c(), tolerance = 1e-7)
})

test_that("cf10", {
  f = generateCF(10, in.dim = 5L, out.dim = 3L)
  value = f(c(1, 1, 1, 1, 1))
  expect_equal(value, c(2.837298, 8, 11.06382), tolerance = 1e-6)
})