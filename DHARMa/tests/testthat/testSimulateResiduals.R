
test_that("Rotation of residuals works", {


  testData = createData(family = gaussian())
  fittedModel <- lm(observedResponse ~ Environment1 , data = testData)

  res1 = simulateResiduals(fittedModel)
  res1 = simulateResiduals(fittedModel, rotation = diag(x = rep(1,100)))

  res2 = recalculateResiduals(res1, group = testData$group)
  res2 = recalculateResiduals(res1, group = testData$group, rotation = diag(x = rep(1,10)))

})

