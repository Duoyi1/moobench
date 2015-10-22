context("moo")

test_that("zdt1", {
  f = generateZDT(1)
  g = generateMOO("zdt1")
  expect_equal(f, g)
})

test_that("zdt2", {
  f = generateZDT(2)
  g = generateMOO("zdt2")
  expect_equal(f, g)
})

test_that("zdt3", {
  f = generateZDT(3)
  g = generateMOO("zdt3")
  expect_equal(f, g)
})

test_that("zdt4", {
  f = generateZDT(4)
  g = generateMOO("zdt4")
  expect_equal(f, g)
})

test_that("zdt5", {
  f = generateZDT(5)
  g = generateMOO("zdt5")
  expect_equal(f, g)
})

test_that("zdt6", {
  f = generateZDT(6)
  g = generateMOO("zdt6")
  expect_equal(f, g)
})

test_that("dtlz1", {
  f = generateDTLZ(1)
  g = generateMOO("dtlz1")
  expect_equal(f, g)
})

test_that("dtlz2", {
  f = generateDTLZ(2)
  g = generateMOO("dtlz2")
  expect_equal(f, g)
})

test_that("dtlz3", {
  f = generateDTLZ(3)
  g = generateMOO("dtlz3")
  expect_equal(f, g)
})

test_that("dtlz4", {
  f = generateDTLZ(4)
  g = generateMOO("dtlz4")
  expect_equal(f, g)
})

test_that("dtlz5", {
  f = generateDTLZ(5)
  g = generateMOO("dtlz5")
  expect_equal(f, g)
})

test_that("dtlz6", {
  f = generateDTLZ(6)
  g = generateMOO("dtlz6")
  expect_equal(f, g)
})

# test_that("dtlz7", {
#   f = generateDTLZ(7)
#   g = generateMOO("dtlz7")
#   expect_equal(f, g)
# })

test_that("lz1", {
  f = generateLZ(1)
  g = generateMOO("lz1")
  expect_equal(f, g)
})

test_that("lz2", {
  f = generateLZ(2)
  g = generateMOO("lz2")
  expect_equal(f, g)
})

test_that("lz3", {
  f = generateLZ(3)
  g = generateMOO("lz3")
  expect_equal(f, g)
})

test_that("lz4", {
  f = generateLZ(4)
  g = generateMOO("lz4")
  expect_equal(f, g)
})

test_that("lz5", {
  f = generateLZ(5)
  g = generateMOO("lz5")
  expect_equal(f, g)
})

test_that("lz6", {
  f = generateLZ(6, out.dim = 3L)
  g = generateMOO("lz6", out.dim = 3L)
  expect_equal(f, g)
})

test_that("lz7", {
  f = generateLZ(7)
  g = generateMOO("lz7")
  expect_equal(f, g)
})

test_that("lz8", {
  f = generateLZ(8)
  g = generateMOO("lz8")
  expect_equal(f, g)
})

test_that("lz9", {
  f = generateLZ(9)
  g = generateMOO("lz9")
  expect_equal(f, g)
})

test_that("uf1", {
  f = generateUF(1)
  g = generateMOO("uf1")
  expect_equal(f, g)
})

test_that("uf2", {
  f = generateUF(2)
  g = generateMOO("uf2")
  expect_equal(f, g)
})

test_that("uf3", {
  f = generateUF(3)
  g = generateMOO("uf3")
  expect_equal(f, g)
})

test_that("uf4", {
  f = generateUF(4)
  g = generateMOO("uf4")
  expect_equal(f, g)
})

test_that("uf5", {
  f = generateUF(5)
  g = generateMOO("uf5")
  expect_equal(f, g)
})

test_that("uf6", {
  f = generateUF(6)
  g = generateMOO("uf6")
  expect_equal(f, g)
})

test_that("uf7", {
  f = generateUF(7)
  g = generateMOO("uf7")
  expect_equal(f, g)
})

test_that("uf8", {
  f = generateUF(8, out.dim = 3L)
  g = generateMOO("uf8", out.dim = 3L)
  expect_equal(f, g)
})

test_that("uf9", {
  f = generateUF(9, out.dim = 3L)
  g = generateMOO("uf9", out.dim = 3L)
  expect_equal(f, g)
})

test_that("uf10", {
  f = generateUF(10, out.dim = 3L)
  g = generateMOO("uf10", out.dim = 3L)
  expect_equal(f, g)
})

test_that("cf1", {
  f = generateCF(1)
  g = generateMOO("cf1")
  expect_equal(f, g)
})

test_that("cf2", {
  f = generateCF(2)
  g = generateMOO("cf2")
  expect_equal(f, g)
})

# Fix me.
# test_that("cf3", {
#   f = generateCF(3)
#   g = generateMOO("cf3")
#   expect_equal(f, g)
# })

test_that("cf4", {
  f = generateCF(4)
  g = generateMOO("cf4")
  expect_equal(f, g)
})

test_that("cf5", {
  f = generateCF(5)
  g = generateMOO("cf5")
  expect_equal(f, g)
})

test_that("cf6", {
  f = generateCF(6)
  g = generateMOO("cf6")
  expect_equal(f, g)
})

test_that("cf7", {
  f = generateCF(7)
  g = generateMOO("cf7")
  expect_equal(f, g)
})

test_that("cf8", {
  f = generateCF(8, out.dim = 3L)
  g = generateMOO("cf8", out.dim = 3L)
  expect_equal(f, g)
})

test_that("cf9", {
  f = generateCF(9, out.dim = 3L)
  g = generateMOO("cf9", out.dim = 3L)
  expect_equal(f, g)
})

test_that("cf10", {
  f = generateCF(10, out.dim = 3L)
  g = generateMOO("cf10", out.dim = 3L)
  expect_equal(f, g)
})

test_that("wfg1", {
  f = generateWFG(1, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg1", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg2", {
  f = generateWFG(2, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg2", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg3", {
  f = generateWFG(3, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg3", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg4", {
  f = generateWFG(4, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg4", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg5", {
  f = generateWFG(5, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg5", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg6", {
  f = generateWFG(6, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg6", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg7", {
  f = generateWFG(7, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg7", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg8", {
  f = generateWFG(8, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg8", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("wfg9", {
  f = generateWFG(9, in.dim = 5L, out.dim = 2L, k = 3L)
  g = generateMOO("wfg9", in.dim = 5L, out.dim = 2L, k = 3L)
  expect_equal(f, g)
})

test_that("mop1", {
  f = generateMOP(1, in.dim = 1L)
  g = generateMOO("mop1", in.dim = 1L)
  expect_equal(f, g)
})

test_that("mop2", {
  f = generateMOP(2)
  g = generateMOO("mop2")
  expect_equal(f, g)
})

test_that("mop3", {
  f = generateMOP(3, in.dim = 2L)
  g = generateMOO("mop3", in.dim = 2L)
  expect_equal(f, g)
})

test_that("mop4", {
  f = generateMOP(4, in.dim = 3L)
  g = generateMOO("mop4", in.dim = 3L)
  expect_equal(f, g)
})

test_that("mop5", {
  f = generateMOP(5, in.dim = 2L, out.dim = 3L)
  g = generateMOO("mop5", in.dim = 2L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("mop6", {
  f = generateMOP(6, in.dim = 2L)
  g = generateMOO("mop6", in.dim = 2L)
  expect_equal(f, g)
})

test_that("mop7", {
  f = generateMOP(7, in.dim = 2L, out.dim = 3L)
  g = generateMOO("mop7", in.dim = 2L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("mop_c1", {
  f = generateMOP_C(1, in.dim = 2L)
  g = generateMOO("mop_c1", in.dim = 2L)
  expect_equal(f, g)
})

test_that("mop_c2", {
  f = generateMOP_C(2, in.dim = 6L)
  g = generateMOO("mop_c2", in.dim = 6L)
  expect_equal(f, g)
})

test_that("mop_c3", {
  f = generateMOP_C(3, in.dim = 2L, out.dim = 3L)
  g = generateMOO("mop_c3", in.dim = 2L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("bk1", {
  f = generateBK1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("bk1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("dgo1", {
  f = generateDGO1(in.dim = 1L, out.dim = 2L)
  g = generateMOO("dgo1", in.dim = 1L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("dgo2", {
  f = generateDGO2(in.dim = 1L, out.dim = 2L)
  g = generateMOO("dgo2", in.dim = 1L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("fa1", {
  f = generateFA1(in.dim = 3L, out.dim = 3L)
  g = generateMOO("fa1", in.dim = 3L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("far1", {
  f = generateFar1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("far1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("fes1", {
  f = generateFES1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("fes1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("fes2", {
  f = generateFES2(in.dim = 2L, out.dim = 3L)
  g = generateMOO("fes2", in.dim = 2L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("fes3", {
  f = generateFES3(in.dim = 2L, out.dim = 4L)
  g = generateMOO("fes3", in.dim = 2L, out.dim = 4L)
  expect_equal(f, g)
})

test_that("ff1", {
  f = generateFF1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("ff1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("ikk1", {
  f = generateIKK1(in.dim = 2L, out.dim = 3L)
  g = generateMOO("ikk1", in.dim = 2L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("im1", {
  f = generateIM1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("im1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("jos1", {
  f = generateJOS1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("jos1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("jos2", {
  f = generateJOS2(in.dim = 2L, out.dim = 2L)
  g = generateMOO("jos2", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("kur1", {
  f = generateKur1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("kur1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("lrs1", {
  f = generateLRS1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("lrs1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("ltdz1", {
  f = generateLTDZ1(in.dim = 3L, out.dim = 3L)
  g = generateMOO("ltdz1", in.dim = 3L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("le1", {
  f = generateLE1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("le1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("mhhm1", {
  f = generateMHHM1(in.dim = 1L, out.dim = 3L)
  g = generateMOO("mhhm1", in.dim = 1L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("mhhm2", {
  f = generateMHHM2(in.dim = 2L, out.dim = 3L)
  g = generateMOO("mhhm2", in.dim = 2L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("mlf1", {
  f = generateMLF1(in.dim = 1L, out.dim = 2L)
  g = generateMOO("mlf1", in.dim = 1L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("mlf2", {
  f = generateMLF2(in.dim = 2L, out.dim = 2L)
  g = generateMOO("mlf2", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("qv1", {
  f = generateQV1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("qv1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("sch1", {
  f = generateSch1(in.dim = 1L, out.dim = 2L)
  g = generateMOO("sch1", in.dim = 1L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("sp1", {
  f = generateSP1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("sp1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("ssfyy1", {
  f = generateSSFYY1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("ssfyy1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("ssfyy2", {
  f = generateSSFYY2(in.dim = 1L, out.dim = 2L)
  g = generateMOO("ssfyy2", in.dim = 1L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("sk1", {
  f = generateSK1(in.dim = 1L, out.dim = 2L)
  g = generateMOO("sk1", in.dim = 1L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("sk2", {
  f = generateSK2(in.dim = 4L, out.dim = 2L)
  g = generateMOO("sk2", in.dim = 4L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("tkly1", {
  f = generateTKLY1(in.dim = 4L, out.dim = 2L)
  g = generateMOO("tkly1", in.dim = 4L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("vu1", {
  f = generateVU1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("vu1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("vu2", {
  f = generateVU2(in.dim = 2L, out.dim = 2L)
  g = generateMOO("vu2", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})

test_that("vfm1", {
  f = generateVFM1(in.dim = 2L, out.dim = 3L)
  g = generateMOO("vfm1", in.dim = 2L, out.dim = 3L)
  expect_equal(f, g)
})

test_that("zlt1", {
  f = generateZLT1(in.dim = 2L, out.dim = 2L)
  g = generateMOO("zlt1", in.dim = 2L, out.dim = 2L)
  expect_equal(f, g)
})