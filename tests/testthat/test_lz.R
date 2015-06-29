context("lz")

test_that("lz1", {
  f = generateLZ1(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1, 0))
})

test_that("lz2", {
  f = generateLZ2(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(3, 0.03589838))
})

test_that("lz3", {
  f = generateLZ3(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(7.48, 0.1887187), tolerance = 1e-7)
})

test_that("lz4", {
  f = generateLZ4(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1.72, 0.1887187))
})

test_that("lz5", {
  f = generateLZ5(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(8.22, 0.7449043))
})

test_that("lz6", {
  f = generateLZ6(in.dim = 5)
  value = f(c(1, 1, 1, 1, 1))
  expect_equal(value, c(0.06165, 2, 2.627616), tolerance = 1e-7)
})

test_that("lz7", {
  f = generateLZ7(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1, 0))
})

test_that("lz8", {
  f = generateLZ8(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1, 0))
})

test_that("lz9", {
  f = generateLZ9(in.dim = 3)
  value = f(c(1, 1, 1))
  expect_equal(value, c(3, 0.03589838))
})