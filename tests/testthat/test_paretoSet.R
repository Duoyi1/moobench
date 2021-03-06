context("paretoSet")

testParetoSet = function(name, in.dim, out.dim, ...) {
  test_that(name, {
    f = generateMOO(name, in.dim, out.dim, ...)
    ps = getParetoSet(f, 100)
    
    if (is.null(attributes(f)$paretoSet)) {
      expect_equal(ps, matrix(NA_real_, ncol = getInDim(f), nrow = 0L))
    } else {
      expect_equal(dim(ps), c(100, in.dim))
      vals = apply(ps, 1, f)
      
      # if our points are numerical identical dominance is random - so exclude
      # points to near to each other
      to.small = as.matrix(dist(t(vals), diag = FALSE, upper = TRUE)) < 1e-6
      diag(to.small) = FALSE
      vals = vals[, !apply(to.small, 2, any)]
      
      expect_true(all(!is_dominated(vals)))
    }
    
  })
}

testParetoSet("zdt1", in.dim = 2L, out.dim = 2L)
testParetoSet("zdt2", in.dim = 2L, out.dim = 2L)
testParetoSet("zdt3", in.dim = 2L, out.dim = 2L)
testParetoSet("zdt4", in.dim = 2L, out.dim = 2L)
testParetoSet("zdt5", in.dim = 2L, out.dim = 2L)
testParetoSet("zdt6", in.dim = 2L, out.dim = 2L)

testParetoSet("dtlz1", in.dim = 2L, out.dim = 2L)
testParetoSet("dtlz2", in.dim = 2L, out.dim = 2L)
testParetoSet("dtlz3", in.dim = 2L, out.dim = 2L)
testParetoSet("dtlz4", in.dim = 2L, out.dim = 2L)
testParetoSet("dtlz5", in.dim = 2L, out.dim = 2L)
testParetoSet("dtlz6", in.dim = 2L, out.dim = 2L)
#testParetoSet("dtlz7", in.dim = 2L, out.dim = 2L)

testParetoSet("lz1", in.dim = 3L, out.dim = 2L)
testParetoSet("lz2", in.dim = 3L, out.dim = 2L)
testParetoSet("lz3", in.dim = 3L, out.dim = 2L) 
testParetoSet("lz4", in.dim = 3L, out.dim = 2L)
testParetoSet("lz5", in.dim = 3L, out.dim = 2L)
testParetoSet("lz6", in.dim = 5L, out.dim = 3L)
testParetoSet("lz7", in.dim = 3L, out.dim = 2L) 
testParetoSet("lz8", in.dim = 3L, out.dim = 2L)
testParetoSet("lz9", in.dim = 3L, out.dim = 2L)

testParetoSet("uf1", in.dim = 3L, out.dim = 2L)
testParetoSet("uf2", in.dim = 3L, out.dim = 2L)
testParetoSet("uf3", in.dim = 3L, out.dim = 2L)
testParetoSet("uf4", in.dim = 3L, out.dim = 2L)
testParetoSet("uf5", in.dim = 3L, out.dim = 2L)
testParetoSet("uf6", in.dim = 3L, out.dim = 2L)
testParetoSet("uf7", in.dim = 3L, out.dim = 2L)
testParetoSet("uf8", in.dim = 5L, out.dim = 3L)
testParetoSet("uf9", in.dim = 5L, out.dim = 3L)
testParetoSet("uf10", in.dim = 5L, out.dim = 3L)

testParetoSet("cf1", in.dim = 3L, out.dim = 2L)
testParetoSet("cf2", in.dim = 3L, out.dim = 2L)
testParetoSet("cf3", in.dim = 3L, out.dim = 2L)
testParetoSet("cf4", in.dim = 3L, out.dim = 2L)
testParetoSet("cf5", in.dim = 3L, out.dim = 2L)
testParetoSet("cf6", in.dim = 4L, out.dim = 2L)
testParetoSet("cf7", in.dim = 4L, out.dim = 2L)
testParetoSet("cf8", in.dim = 5L, out.dim = 3L)
testParetoSet("cf9", in.dim = 5L, out.dim = 3L)
testParetoSet("cf10", in.dim = 5L, out.dim = 3L)

testParetoSet("wfg1", in.dim = 5L, out.dim = 2L, k = 3L)
testParetoSet("wfg2", in.dim = 5L, out.dim = 2L, k = 3L)
testParetoSet("wfg3", in.dim = 5L, out.dim = 2L, k = 3L)
testParetoSet("wfg4", in.dim = 5L, out.dim = 2L, k = 3L)
testParetoSet("wfg5", in.dim = 5L, out.dim = 2L, k = 3L)
testParetoSet("wfg6", in.dim = 5L, out.dim = 2L, k = 3L)
testParetoSet("wfg7", in.dim = 5L, out.dim = 2L, k = 3L)
#testParetoSet("wfg8", in.dim = 5L, out.dim = 2L, k = 3L)
testParetoSet("wfg9", in.dim = 5L, out.dim = 2L, k = 3L)

testParetoSet("mop1", in.dim = 1L, out.dim = 2L)
testParetoSet("mop2", in.dim = 2L, out.dim = 2L)
testParetoSet("mop3", in.dim = 2L, out.dim = 2L)
testParetoSet("mop4", in.dim = 3L, out.dim = 2L)
testParetoSet("mop5", in.dim = 2L, out.dim = 3L)
testParetoSet("mop6", in.dim = 2L, out.dim = 2L)
testParetoSet("mop7", in.dim = 2L, out.dim = 3L)
testParetoSet("mop_c1", in.dim = 2L, out.dim = 2L)
testParetoSet("mop_c2", in.dim = 6L, out.dim = 2L)
testParetoSet("mop_c3", in.dim = 2L, out.dim = 3L)

testParetoSet("bk1", in.dim = 2L, out.dim = 2L)
testParetoSet("dgo1", in.dim = 1L, out.dim = 2L)
testParetoSet("dgo2", in.dim = 1L, out.dim = 2L)
testParetoSet("fa1", in.dim = 3L, out.dim = 3L)
testParetoSet("far1", in.dim = 2L, out.dim = 2L)
testParetoSet("fes1", in.dim = 2L, out.dim = 2L)
testParetoSet("fes2", in.dim = 2L, out.dim = 3L)
testParetoSet("fes3", in.dim = 2L, out.dim = 4L)
testParetoSet("ff1", in.dim = 2L, out.dim = 2L)
testParetoSet("ikk1", in.dim = 2L, out.dim = 3L)
testParetoSet("im1", in.dim = 2L, out.dim = 2L)
testParetoSet("jos1", in.dim = 2L, out.dim = 2L)
testParetoSet("jos2", in.dim = 2L, out.dim = 2L)
testParetoSet("kur1", in.dim = 2L, out.dim = 2L)
testParetoSet("lrs1", in.dim = 2L, out.dim = 2L)
testParetoSet("ltdz1", in.dim = 3L, out.dim = 3L)
testParetoSet("le1", in.dim = 2L, out.dim = 2L)
testParetoSet("mhhm1", in.dim = 1L, out.dim = 3L)
testParetoSet("mhhm2", in.dim = 2L, out.dim = 3L)
testParetoSet("mlf1", in.dim = 1L, out.dim = 2L)
testParetoSet("mlf2", in.dim = 2L, out.dim = 2L)
testParetoSet("qv1", in.dim = 2L, out.dim = 2L)
testParetoSet("sch1", in.dim = 1L, out.dim = 2L)
testParetoSet("sp1", in.dim = 2L, out.dim = 2L)
testParetoSet("ssfyy1", in.dim = 2L, out.dim = 2L)
testParetoSet("ssfyy2", in.dim = 1L, out.dim = 2L)
testParetoSet("sk1", in.dim = 1L, out.dim = 2L)
testParetoSet("sk2", in.dim = 4L, out.dim = 2L)
testParetoSet("tkly1", in.dim = 4L, out.dim = 2L)
testParetoSet("vu1", in.dim = 2L, out.dim = 2L)
testParetoSet("vu2", in.dim = 2L, out.dim = 2L)
testParetoSet("vfm1", in.dim = 2L, out.dim = 3L)
testParetoSet("zlt1", in.dim = 2L, out.dim = 2L)

