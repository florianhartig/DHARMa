test_that("getPredictorNames for brms models (#542, #573)", {
  skip_if_not_installed("brms")
  set.seed(1)
  d = data.frame(y = rpois(30, 3) + 1, x = rnorm(30), z = runif(30, 1, 2), w = runif(30))
  # empty = TRUE: model object without compilation / sampling
  getNames = function(f) getPredictorNames(suppressMessages(brms::brm(f, data = d, empty = TRUE)))
  expect_equal(getNames(y ~ x), "x")
  expect_equal(getNames(y ~ x + z), c("x", "z"))
  expect_equal(getNames(log(y) ~ x + I(x^2)), c("x", "I(x^2)"))
  expect_equal(getNames(y | weights(w) ~ x + z), c("x", "z"))
  expect_setequal(getNames(y | weights(w) ~ x + w), c("w", "x"))
  expect_setequal(getNames(brms::bf(y | weights(w) ~ x, sigma ~ w)), c("w", "x"))
})
