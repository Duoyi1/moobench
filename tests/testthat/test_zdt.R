context("zdt")

test_that("zdt1", {
  f = generateZDT1(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(1, 10 * (1 - sqrt(0.1))))
})
