\dontrun{

  library(DHARMa)
  library(phylolm)

  set.seed(123)
  tre = rcoal(60)
  b0 = 0; b1 = 1;
  x <- runif(length(tre$tip.label), 0, 1)
  y <- b0 + b1*x +
    rTrait(n = 1, phy = tre,model="BM",
           parameters = list(ancestral.state = 0, sigma2 = 10))
  dat = data.frame(trait = y, pred = x)

  fit = lm(trait ~ pred, data = dat)
  res = simulateResiduals(fit, plot = T)

  testPhylogeneticAutocorrelation(res, tree = tre)


  fit = phylolm(trait ~ pred, data = dat, phy = tre, model = "BM")
  summary(fit)

  # phylogenetic autocorrelation still present in residuals
  res = simulateResiduals(fit, plot = T)

  # with "rotation" the residual autcorrelation is gone, see ?simulateResiduals.
  res = simulateResiduals(fit, plot = T, rotation = "estimated")

}
