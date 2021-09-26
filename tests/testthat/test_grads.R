context("grads")


tanh <- function(x, y) {
  (1.0 - exp(-x)) / (1.0 + exp(-y))
}


logpdf <- function(y, mu, sigma) {
  -0.5 * ((y - mu) / sigma) ^ 2 - log(sigma) - 0.5 * log(2 * pi)
}


test_that("tanh produces correct gradients for first arg", {
  g <- grad(tanh, 1L)(1.0, 2.0)
  testthat::expect_equal(g, 0.3240271368319427, tolerance = 0.001)
})


test_that("tanh produces correc gradients for second arg", {
  g <- grad(tanh, 2L)(1.0, 2.0)
  testthat::expect_equal(g, 0.06636860387867843, tolerance = 0.001)
})


test_that("log dnorm produces correct gradients for y", {
  g <- grad(logpdf, 1L)(0.98, 0.1, 2.0)
  testthat::expect_equal(g, -0.22, tolerance = 0.001)
})


test_that("log dnorm produces correct gradients for mu", {
  g <- grad(logpdf, 2L)(0.98, 0.1, 2.0)
  testthat::expect_equal(g, 0.22, tolerance = 0.001)
})

test_that("log dnorm produces correct gradients for sigma", {
  g <- grad(logpdf, 3L)(0.98, 0.1, 2.0)
  testthat::expect_equal(g, -0.4032, tolerance = 0.001)
})

