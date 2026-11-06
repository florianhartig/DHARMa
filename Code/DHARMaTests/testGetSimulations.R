test_that("brms: unconditional simulations stop for unsupported models (#574)", {
  skip_if_not_installed("brms")
  set.seed(1)
  d = data.frame(y = rpois(30, 3), x = rnorm(30), b = factor(rep(1:10, 3)), bn = rep(1:10, 3), z = factor(rep(rep(1:2, each = 5), 3)))
  A = diag(10); dimnames(A) = list(levels(d$b), levels(d$b))
  # empty = TRUE: model object without compilation / sampling
  getSim = function(f, ...) getSimulations(suppressMessages(brms::brm(f, data = d, family = poisson, empty = TRUE, ...)), simulateREs = "unconditional")
  expect_error(getSim(y ~ x + (1|gr(b, cov = A)), data2 = list(A = A)), "gr\\(\\.\\.\\., cov")
  expect_error(getSim(y ~ x + (1|gr(b, by = z))), "gr\\(\\.\\.\\., by")
  # grouping variable also used as a fixed effect
  expect_error(getSim(y ~ b + (1|b)), "also used outside the random effects")
  expect_error(getSim(y ~ bn + (1|bn)), "also used outside the random effects")
})
