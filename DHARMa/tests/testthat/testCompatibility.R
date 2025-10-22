test_that("Conditional simulations have a smaller spread than unconditional simulations", {

  # create data
  # poisson, because required for GLMMadaptive; increased RE variance
  set.seed(123)
  testData = createData(100, randomEffectVariance = 2)

  # fit models
  mglmer = lme4::glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mglmmTMB = glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mspaMM = spaMM::HLfit(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mGLMMadaptive = GLMMadaptive::mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 |group, data = testData, family = poisson())

  # get simulations and their SD
  glmer_cond = apply(getSimulations(mglmer, simulateREs = "conditional", nsim = 100), 2, sd)
  glmer_uncond = apply(getSimulations(mglmer, simulateREs = "unconditional", nsim = 100), 2, sd)

  glmmTMB_cond = apply(getSimulations(mglmmTMB, simulateREs = "conditional", nsim = 100), 2, sd)
  glmmTMB_uncond = apply(getSimulations(mglmmTMB, simulateREs = "unconditional", nsim = 100), 2, sd)

  spaMM_cond = apply(getSimulations(mspaMM, simulateREs = "conditional", nsim = 100), 2, sd)
  spaMM_uncond = apply(getSimulations(mspaMM, simulateREs = "unconditional", nsim = 100), 2, sd)

  GLMMadaptive_cond = apply(getSimulations(mGLMMadaptive, simulateREs = "conditional", nsim = 100), 2, sd)
  GLMMadaptive_uncond = apply(getSimulations(mGLMMadaptive, simulateREs = "unconditional", nsim = 100), 2, sd)

  # expect lower spread for conditional simulations
  expect_true(sd(glmer_cond) < sd(glmer_uncond))
  expect_true(sd(glmmTMB_cond) < sd(glmmTMB_uncond))
  expect_true(sd(spaMM_cond) < sd(spaMM_uncond))
  expect_true(sd(GLMMadaptive_cond) < sd(GLMMadaptive_uncond))

})





test_that("Unconditional predictions are the default in DHARMa", {

  # create data
  # poisson, because required for GLMMadaptive; increased RE variance
  testData = createData(100, randomEffectVariance = 2)

  # fit models
  mglmer = lme4::glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mglmmTMB = glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mspaMM = spaMM::HLfit(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  mGLMMadaptive = GLMMadaptive::mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 |group, data = testData, family = poisson())

  # compare default getFitted() and unconditional predictions
  # following the package-specific syntax and using predict()
  glmer_fitted = getFitted(mglmer)
  glmer_predict = predict(mglmer, re.form = ~0, type = "response")
  expect_equal(glmer_fitted, unname(glmer_predict))

  glmmTMB_fitted = getFitted(mglmmTMB)
  glmmTMB_predict = predict(mglmmTMB, re.form = ~0, type = "response")
  expect_equal(glmmTMB_fitted, unname(glmmTMB_predict))

  spaMM_fitted = getFitted(mspaMM)
  spaMM_predict = predict(mspaMM, re.form = NA)
  expect_equal(spaMM_fitted, spaMM_predict[,1])

  GLMMadaptive_fitted = getFitted(mGLMMadaptive)
  GLMMadaptive_predict = predict(mGLMMadaptive, type = "mean_subject")
  expect_equal(GLMMadaptive_fitted, GLMMadaptive_predict)

})


