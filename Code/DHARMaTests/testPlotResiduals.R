# counts the plots (panels) drawn when evaluating expr
countPanels = function(expr){
  n = 0
  setHook("plot.new", function() n <<- n + 1)
  on.exit(setHook("plot.new", NULL, "replace"))
  expr
  n
}

test_that("plotResiduals with form = ~x | group plots each group level (#573)", {
  set.seed(123)
  testData = createData(sampleSize = 200, randomEffectVariance = 0, numGroups = 10)
  id = as.integer(testData$group)
  testData$unused = factor(testData$group, levels = c(levels(testData$group), "unused"))
  testData$letters = factor(letters[id])
  testData$numeric = id * 1.5
  testData$exponent = factor(id * 1e5)
  testData$plus = factor(paste0(letters[id], "+1"))
  testData$withNA = factor(ifelse(id == 1, NA, id))
  fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel, n = 100)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  for(group in c("unused", "letters", "numeric", "exponent", "plus")){
    form = as.formula(paste("~Environment1 |", group))
    expect_equal(countPanels(plotResiduals(simulationOutput, form = form, quantreg = FALSE)), 10, label = group)
  }
  expect_equal(countPanels(plotResiduals(simulationOutput, form = ~Environment1 | withNA, quantreg = FALSE)), 9)
})
