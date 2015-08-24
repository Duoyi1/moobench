context("wfg")

source("tests/testthat/wfg_cmp_impl.R")

test_that("wfg1", {
  x = runif(5)
  f = generateWFG(1, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg1(x))
})

test_that("wfg2", {
  x = runif(5)
  f = generateWFG(2, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg2(x))
})

test_that("wfg3", {
  x = runif(5)
  f = generateWFG(3, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg3(x))
})

test_that("wfg4", {
  x = runif(5)
  f = generateWFG(4, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg4(x))
})

test_that("wfg5", {
  x = runif(5)
  f = generateWFG(5, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg5(x))
})

test_that("wfg6", {
  x = runif(5)
  f = generateWFG(6, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg6(x))
})

test_that("wfg7", {
  x = runif(5)
  f = generateWFG(7, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg7(x))
})

test_that("wfg8", {
  x = runif(5)
  f = generateWFG(8, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg8(x))
})

test_that("wfg9", {
  x = runif(5)
  f = generateWFG(9, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg9(x))
})
