context("other wfg")

test_that("bk1", {
  f = generateBK1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(2, 32))
})

test_that("dgo1", {
  f = generateDGO1(in.dim = 1L, out.dim = 2L)
  value = f(1)
  expect_equal(value, c(sin(1), sin(1.7)))
})

test_that("dgo2", {
  f = generateDGO2(in.dim = 1L, out.dim = 2L)
  value = f(1)
  expect_equal(value, c(1, 9 - sqrt(80)))
})

test_that("fa1", {
  f = generateFA1(in.dim = 3L, out.dim = 3L)
  value = f(c(1, 1, 1))
  expect_equal(value, c(1, 2 * (1 - 0.5^0.5), 2 * (1 - 0.5^0.1)))
})

test_that("far1", {
  f = generateFar1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(-1.661557e-03, 3.043248e-05), tolerance = 1e-6)
})

test_that("fes1", {
  f = generateFES1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1.062742, 1), tolerance = 1e-6)
})

test_that("fes2", {
  f = generateFES2(in.dim = 2L, out.dim = 3L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 1.890671, 1.245796), tolerance = 1e-6)
})

test_that("fes3", {
  f = generateFES3(in.dim = 2L, out.dim = 4L)
  value = f(c(1, 1))
  expect_equal(value, c(1.062742, 1.890671, 1.245796, 0.5), tolerance = 1e-6)
})

test_that("ff1", {
  f = generateFF1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(0.9816844, 0.9816844), tolerance = 1e-6)
})

test_that("ikk1", {
  f = generateIKK1(in.dim = 2L, out.dim = 3L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 19^2, 1))
})

test_that("im1", {
  f = generateIM1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(2, 5))
})

test_that("jos1", {
  f = generateJOS1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 1))
})

test_that("jos2", {
  f = generateJOS2(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 4.375587), tolerance = 1e-6)
})

test_that("kur1", {
  f = generateKur1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(-7.536383,  7.958232), tolerance = 1e-6)
})

test_that("lrs1", {
  f = generateLRS1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(2, 10))
})

test_that("ltdz1", {
  f = generateLTDZ1(in.dim = 3L, out.dim = 3L)
  value = f(c(1, 1, 1))
  expect_equal(value, c(3, 3, 1))
})

test_that("le1", {
  f = generateLE1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1.0905077, 0.8408964), tolerance = 1e-6)
})

test_that("mhhm1", {
  f = generateMHHM1(in.dim = 1L, out.dim = 3L)
  value = f(1)
  expect_equal(value, c(0.04, 0.0225, 0.01))
})

test_that("mhhm2", {
  f = generateMHHM2(in.dim = 2L, out.dim = 3L)
  value = f(c(1, 1))
  expect_equal(value, c(0.2, 0.1125, 0.17))
})

test_that("mlf1", {
  f = generateMLF1(in.dim = 1L, out.dim = 2L)
  value = f(1)
  expect_equal(value, 1.05 * c(sin(1), cos(1)))
})

test_that("mlf2", {
  f = generateMLF2(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(4.47, 4.87))
})

test_that("qv1", {
  f = generateQV1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 2.12132), tolerance = 1e-6)
})

test_that("sch1", {
  f = generateSch1(in.dim = 1L, out.dim = 2L)
  value = f(1)
  expect_equal(value, c(-1, 16))
})

test_that("sp1", {
  f = generateSP1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(0, 4))
})

test_that("ssfyy1", {
  f = generateSSFYY1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(2, 1))
})

test_that("ssfyy2", {
  f = generateSSFYY2(in.dim = 1L, out.dim = 2L)
  value = f(1)
  expect_equal(value, c(11, 9))
})

test_that("sk1", {
  f = generateSK1(in.dim = 1L, out.dim = 2L)
  value = f(1)
  expect_equal(value, c(26, 6.5))
})

test_that("sk2", {
  f = generateSK2(in.dim = 4L, out.dim = 2L)
  value = f(rep(1, 4))
  expect_equal(value, c(-37, 3.236427), tolerance = 1e-6)
})

test_that("tkly1", {
  f = generateTKLY1(in.dim = 4L, out.dim = 2L)
  value = f(rep(1, 4))
  expect_equal(value, c(1, 1.94596), tolerance = 1e-6)
})

test_that("vu1", {
  f = generateVU1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1/3, 5))
})

test_that("vu2", {
  f = generateVU2(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(3, 2))
})

test_that("vfm1", {
  f = generateVFM1(in.dim = 2L, out.dim = 3L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 6, 3))
})

test_that("zlt1", {
  f = generateZLT1(in.dim = 2L, out.dim = 2L)
  value = f(c(1, 1))
  expect_equal(value, c(1, 1))
})