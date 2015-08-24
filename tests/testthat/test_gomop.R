context("gomop")

# test_that("gomop1", {
#   f = generateGOMOP(in.dim = 30L, soobench.funs = list(ackley_function(30L), discus_function(30L),
#     ellipsoidal_function(30L), griewank_function(30L)))
#   values = f(rep(1, 30))
#   ackley = ackley_function(30L)
#   discus = discus_function(30L)
#   ellipsoidal = ellipsoidal_function(30L)
#   griewank = griewank_function(30L)
#   expect_equal(values, c(ackley(rep(32.786, 30)), discus(rep(32.768, 30)), ellipsoidal(rep(32.786, 30)), 
#    griewank(rep(600, 30))))
# })
# 
# test_that("gomop2", {
#   f = generateGOMOP(in.dim = 30L, soobench.funs = list(kotancheck_function(30L), mexican_hat_function(30L),
#     rastrigin_function(30L), rosenbrock_function(30L), weierstrass_function(30L)))
#   values = f(rep(1, 30))
#   kotancheck = kotancheck_function(30L)
#   mexican_hat = mexican_hat_function(30L)
#   rastrigin = rastrigin_function(30L)
#   rosenbrock = rosenbrock_function(30L)
#   weierstrass = weierstrass_function(30L)
#   expect_equal(values, c(kotancheck(c(7, 3, rep(5, 28))), mexican_hat(rep(5, 30)), rastrigin(rep(5, 30)), 
#     rosenbrock(rep(5, 30)), weierstrass(rep(0.5, 30))))
# })
