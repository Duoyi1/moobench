context("mop")

test_that("mop1", {
  f = generateMOP(1, in.dim = 1L)
  value = f(1)
  expect_equal(value, c(1, 1))
})

test_that("mop2", {
  f = generateMOP(2, in.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(0.1576611, 0.9970573), tolerance = 1e-6)
})

test_that("mop3", {
  f = generateMOP(3, in.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(-3.634352, -20),  tolerance = 1e-6)
})

test_that("mop4", {
  f = generateMOP(4, in.dim = 3L)
  value = f(c(1, 1, 1))
  expect_equal(value, c(-15.07277,  15.62206), tolerance = 1e-6)
})

test_that("mop5", {
  f = generateMOP(5, in.dim = 2L, out.dim = 3L)
  value = f(c(1, 1))
  expect_equal(value, c(1.9092974, 18.162037, 0.1844645), tolerance = 1e-6)
})

test_that("mop6", {
  f = generateMOP(6, in.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 10.90909), tolerance = 1e-6)
})

test_that("mop7", {
  f = generateMOP(7, in.dim = 2L, out.dim = 3L)
  value = f(c(1, 1))
  expect_equal(value, c(3.807692, -16.472222, -12.918319), tolerance = 1e-6)
})

test_that("mop_c1", {
  f = generateMOP_C(1, in.dim = 2L)
  value = f(c(0, 1))
  expect_equal(value, c(4, 41))
})

test_that("mop_c2", {
  f = generateMOP_C(2, in.dim = 6L)
  value = f(rep(1, 6))
  expect_equal(value, c(-35, 6))
})

test_that("mop_c3", {
  f = generateMOP_C(3, in.dim = 2L, out.dim = 3L)
  value = f(c(1, 1))
  expect_equal(value, c(3.807692, -12.935462, 18.162037),  tolerance = 1e-6)
})
