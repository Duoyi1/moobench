context("zdt")

test_that("zdt1", {
  f = generateZDT1(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(1, 10 * (1 - sqrt(0.1))))
})

test_that("zdt2", {
  f = generateZDT2(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(1, 10 * (1 - 0.01)))
})

test_that("zdt3", {
  f = generateZDT3(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(1, 10 * (1 - sqrt(0.1) - 0.1 * sin(10 * pi))))
})

test_that("zdt4", {
  f = generateZDT4(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(1, 2 * (1 - sqrt(0.5))))
})

test_that("zdt6", {
  f = generateZDT6(in.dim = 2)
  value = f(c(1, 1))
  expect_equal(value, c(1, 10 * (1 - (0.1)^2)))
})
