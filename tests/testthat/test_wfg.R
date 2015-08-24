context("wfg")

source("wfg_cmp_impl.R")

test_that("wfg1", {
  x = runif(5)
  f = generateWFG(1, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg1(x))
})

test_that("wfg2", {
  x = runif(5)
  f = generateWFG(2, in.dim = 5, out.dim = 2, k = 3)
  expect_equal(f(x), wfg1(x))
})