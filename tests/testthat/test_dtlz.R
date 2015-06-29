context("dtlz")

test_that("dtlz1", {
  f = generateDTLZ1(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(13, 0))
})

test_that("dtlz2", {
  f = generateDTLZ2(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(0, 1.25))
})

test_that("dtlz3", {
  f = generateDTLZ3(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(0, 26))
})

test_that("dtlz4", {
  f = generateDTLZ4(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(0, 1.25))
})

test_that("dtlz5", {
  f = generateDTLZ5(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(0, 1.933033))
})

test_that("dtlz6", {
  f = generateDTLZ6(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(1, 21))
})