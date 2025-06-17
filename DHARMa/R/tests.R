#' DHARMa general residual test
#'
#' Calls uniformity, dispersion and outliers tests.
#'
#' This function is a wrapper for the various test functions implemented in DHARMa. Currently, this function calls the functions [testUniformity], [testDispersion], and [testOutliers]. All other tests (see list below) have to be called by hand.
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param plot if TRUE, plots functions of the tests are called.
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @example inst/examples/testsHelp.R
#' @export
testResiduals <- function(simulationOutput, plot = TRUE){

  opar = par(mfrow = c(1,3))
  on.exit(par(opar))
  out = list()
  out$uniformity = testUniformity(simulationOutput, plot = plot)
  out$dispersion = testDispersion(simulationOutput, plot = plot)
  out$outliers = testOutliers(simulationOutput, plot = plot)

  #print(out) # do we need it?
  return(out)
}

#' Residual tests
#'
#' @details Deprecated, switch your code to using the [testResiduals] function
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @author Florian Hartig
#' @export
testSimulatedResiduals <- function(simulationOutput){
  message("testSimulatedResiduals is deprecated, switch your code to using the testResiduals function")
  testResiduals(simulationOutput)
}


#' Test for overall uniformity
#'
#' This function tests the overall uniformity of the simulated residuals in a DHARMa object
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis. See [stats::ks.test] for details
#' @param plot if TRUE, plots calls [plotQQunif] as well
#' @details The function applies a [stats::ks.test] for uniformity on the simulated residuals.
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @example inst/examples/testsHelp.R
#' @export
testUniformity <- function(simulationOutput, alternative = c("two.sided", "less", "greater"), plot = TRUE){

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif', alternative = alternative))
  if(plot == T) plotQQunif(simulationOutput = simulationOutput)
  return(out)
}


# Experimental
testBivariateUniformity <- function(simulationOutput, alternative = c("two.sided", "less", "greater"), plot = TRUE){

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  #out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif', alternative = alternative))
  #if(plot == T) plotQQunif(simulationOutput = simulationOutput)
  out = NULL
  return(out)
}



#' Test for quantiles
#'
#' This function tests
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param predictor an optional predictor variable to be used, instead of the predicted response (default)
#' @param quantiles the quantiles to be tested
#' @param plot if TRUE, the function will create an additional plot
#' @details The function fits quantile regressions (via package qgam) on the residuals, and compares their location to the expected location (because of the uniform distributionm, the expected location is 0.5 for the 0.5 quantile).
#'
#' A significant p-value for the splines means the fitted spline deviates from a flat line at the expected location (p-values of intercept and spline are combined via Benjamini & Hochberg adjustment to control the FDR)
#'
#' The p-values of the splines are combined into a total p-value via Benjamini & Hochberg adjustment to control the FDR.
#'
#' @author Florian Hartig
#' @example inst/examples/testQuantilesHelp.R
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @export
testQuantiles <- function(simulationOutput, predictor = NULL, quantiles = c(0.25,0.5,0.75), plot = TRUE){

  if(plot == F){

    out = list()
    out$data.name = deparse(substitute(simulationOutput))

    simulationOutput = ensureDHARMa(simulationOutput, convert = T)
    res = simulationOutput$scaledResiduals
    pred = ensurePredictor(simulationOutput, predictor)

    dat = data.frame(res = simulationOutput$scaledResiduals, pred = pred)

    quantileFits <- list()
    pval = rep(NA, length(quantiles))
    predictions = data.frame(pred = sort(dat$pred))
    predictions = cbind(predictions, matrix(ncol = 2 * length(quantiles),
                                            nrow = nrow(dat)))
    for(i in 1:length(quantiles)){
      datTemp = dat
      datTemp$res = datTemp$res - quantiles[i]

      # settings for k = the dimension of the basis used to represent the smooth term.
      # see https://github.com/mfasiolo/qgam/issues/37
      dimSmooth =  min(length(unique(datTemp$pred)), 10)
      quantResult = try(capture.output(quantileFits[[i]] <-
                                         qgam::qgam(res ~ s(pred, k = dimSmooth),
                                                    data = datTemp,
                                                    qu = quantiles[i])), silent = T)
      if(inherits(quantResult, "try-error")){
        message("\n DHARMa: qgam was unable to calculate quantile regression for quantile ",
                quantiles[i], ". Possibly to few (unique) data points / predictions. The quantile will be ommited in plots and significance calculations. \n")
      } else {
        x = summary(quantileFits[[i]])
        pval[i] = min(p.adjust(c(x$p.table[1,4], x$s.table[1,4]), method = "BH")) # correction for test on slope and intercept
        quantPre = predict(quantileFits[[i]], newdata = predictions, se = T)
        predictions[, 2*i] = quantPre$fit + quantiles[i]
        predictions[, 2*i + 1] = quantPre$se.fit
      }
    }

    out$method = "Test for location of quantiles via qgam"
    out$alternative = "both"
    out$pvals = pval
    out$p.value = min(p.adjust(pval, method = "BH")) # correction for multiple quantile tests
    out$predictions = predictions
    out$qgamFits = quantileFits

    class(out) = "htest"

  } else if(plot == T) {
    out <- plotResiduals(simulationOutput = simulationOutput, form = predictor,
                         quantiles = quantiles, quantreg = TRUE)
  }
  return(out)
}


#unif.2017YMi(X, type = c("Q1", "Q2", "Q3"), lower = rep(0, ncol(X)),upper = rep(1, ncol(X)))

#' Test for outliers
#'
#' This function tests if the number of observations outside the simulatio envelope are larger or smaller than expected
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" (default) compared to the simulated null hypothesis
#' @param margin whether to test for outliers only at the lower, only at the upper, or both sides (default) of the simulated data distribution
#' @param type either default, bootstrap or binomial. See details
#' @param nBoot number of boostrap replicates. Only used ot type = "bootstrap"
#' @param plot if TRUE, the function will create an additional plot
#' @param plotBoostrap if plot should be produced of outlier frequencies calculated under the bootstrap
#' @details DHARMa residuals are created by simulating from the fitted model, and comparing the simulated values to the observed data. It can occur that all simulated values are higher or smaller than the observed data, in which case they get the residual value of 0 and 1, respectively. I refer to these values as simulation outliers, or simply outliers.
#'
#' Because no data was simulated in the range of the observed value, we don't know "how strongly" these values deviate from the model expectation, so the term "outlier" should be used with a grain of salt. It is not a judgment about the magnitude of the residual deviation, but simply a dichotomous sign that we are outside the simulated range. Moreover, the number of outliers will decrease as we increase the number of simulations.
#'
#' To test if the outliers are a concern, testOutliers implements 2 options (bootstrap, binomial), which can be chosen via the parameter "type". The third option (default) chooses bootstrap for integer-valued distributions with nObs < 500, and else binomial.
#'
#' The binomial test considers that under the null hypothesis that the model is correct, and for continuous distributions (i.e. data and the model distribution are identical and continous), the probability that a given observation is higher than all simulations is 1/(nSim +1), and binomial distributed. The testOutlier function can test this null hypothesis via type = "binomial". In principle, it would be nice if we could extend this idea to integer-valued distributions, which are randomized via the PIT procedure (see [simulateResiduals]), the rate of "true" outliers is more difficult to calculate, and in general not 1/(nSim +1). The testOutlier function implements a small tweak that calculates the rate of residuals that are closer than 1/(nSim+1) to the 0/1 border, which roughly occur at a rate of nData /(nSim +1). This approximate value, however, is generally not exact, and may be particularly off non-bounded integer-valued distributions (such as Poisson or Negative Binomial).
#'
#' For this reason, the testOutlier function implements an alternative procedure that uses the bootstrap to generate a simulation-based expectation for the outliers. It is recommended to use the bootstrap for integer-valued distributions (and integer-valued only, because it has no advantage for continuous distributions, ideally with reasonably high values of nSim and nBoot (I recommend at least 1000 for both). Because of the high runtime, however, this option is switched off for type = default when nObs > 500.
#'
#' Both binomial or bootstrap generate a null expectation, and then test for an excess or lack of outliers. Per default, testOutliers() looks for both, so if you get a significant p-value, you have to check if you have to many or too few outliers. An excess of outliers is to be interpreted as too many values outside the simulation envelope. This could be caused by overdispersion, or by what we classically call outliers. A lack of outliers would be caused, for example, by underdispersion.
#'
#'
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @export
testOutliers <- function(simulationOutput, alternative = c("two.sided", "greater", "less"), margin = c("both", "upper", "lower"), type = c("default","bootstrap", "binomial"), nBoot = 100, plot = TRUE, plotBoostrap = FALSE){

  # check inputs
  alternative = match.arg(alternative)
  margin = match.arg(margin)
  type = match.arg(type)
  data.name = deparse(substitute(simulationOutput)) # remember: needs to be called before ensureDHARMa
  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  if(type == "default"){
    if(simulationOutput$integerResponse == FALSE) type = "binomial"
    else{
      if(simulationOutput$nObs > 500) type = "binomial"
      else type = "bootstrap"
    }
  }

  # using the binomial test, not exact
  if(type == "binomial"){

    # calculation of outliers
    if(margin == "both")  outliers = sum(simulationOutput$scaledResiduals < (1/(simulationOutput$nSim+1))) + sum(simulationOutput$scaledResiduals > (1-1/(simulationOutput$nSim+1)))
    if(margin == "upper") outliers = sum(simulationOutput$scaledResiduals > (1-1/(simulationOutput$nSim+1)))
    if(margin == "lower") outliers = sum(simulationOutput$scaledResiduals < (1/(simulationOutput$nSim+1)))

    # calculations of trials and H0
    outFreqH0 = 1/(simulationOutput$nSim +1) * ifelse(margin == "both", 2, 1)
    trials = simulationOutput$nObs

    out = binom.test(outliers, trials, p = outFreqH0, alternative = alternative)

    # overwrite information in binom.test
    out$method = "DHARMa outlier test based on exact binomial test with approximate expectations"
    out$data.name = data.name
    out$margin = margin
    names(out$statistic) = paste("outliers at", margin, "margin(s)")
    names(out$parameter) = "observations"
    names(out$estimate) = paste("frequency of outliers (expected:", out$null.value,")")

    if (simulationOutput$integerResponse == T & out$p.value < 0.05) message("DHARMa:testOutliers with type = binomial may have inflated Type I error rates for integer-valued distributions. To get a more exact result, it is recommended to re-run testOutliers with type = 'bootstrap'. See ?testOutliers for details")

    if(plot == T) {

      hist(simulationOutput, main = "")

      main = ifelse(out$p.value <= 0.05,
                    "Outlier test significant",
                    "Outlier test n.s.")

      title(main = main, cex.main = 1,
            col.main = ifelse(out$p.value <= 0.05, "red", "black"))

    }

  } else {

    if(margin == "both")  outliers = mean(simulationOutput$scaledResiduals == 0) +
        mean(simulationOutput$scaledResiduals == 1)
    if(margin == "upper") outliers = mean(simulationOutput$scaledResiduals == 1)
    if(margin == "lower") outliers = mean(simulationOutput$scaledResiduals == 0)


    # Bootstrapping to compare to expected

    simIndices = 1:simulationOutput$nSim
    nSim = simulationOutput$nSim
    if(simulationOutput$refit == T){
      simResp = simulationOutput$refittedResiduals
    } else {
      simResp = simulationOutput$simulatedResponse
    }
    resMethod = simulationOutput$method
    resInteger = simulationOutput$integerResponse


    if (nBoot > nSim){
      message("DHARMa::testOutliers: nBoot > nSim does not make much sense, thus changed to nBoot = nSim. If you want to increase nBoot, increase nSim in DHARMa::simulateResiduals as well.")
      nBoot = nSim
    }

    frequBoot <- rep(NA, nBoot)

    for (i in 1:nBoot){

      #sel = -i
      sel = sample(simIndices[-i], size = nSim, replace = T)

      residuals <- getQuantile(simulations = simResp[,sel],
                               observed = simResp[,i],
                               integerResponse = resInteger,
                               method = resMethod)

      if(margin == "both")  frequBoot[i] = mean(residuals == 1) + mean(residuals == 0)
      else if(margin == "upper") frequBoot[i] = mean(residuals == 1)
      else if(margin == "lower") frequBoot[i] = mean(residuals == 0)
    }

    out = list()
    class(out) = "htest"
    out$alternative = alternative
    out$p.value = getP(frequBoot, outliers, alternative = alternative)

    out$conf.int = quantile(frequBoot, c(0.025, 0.975))

    out$data.name = data.name
    out$margin = margin

    out$method = "DHARMa bootstrapped outlier test"

    out$statistic = outliers * simulationOutput$nObs
    names(out$statistic) = paste("outliers at", margin, "margin(s)")
    out$parameter = simulationOutput$nObs
    names(out$parameter) = "observations"
    out$estimate = outliers
    names(out$estimate) = paste("outlier frequency (expected:", mean(frequBoot),")")

    if(plotBoostrap == T){
      hist(frequBoot, xlim = range(frequBoot, outliers), col = "lightgrey", main = "Bootstrapped outlier frequency")
      abline(v = mean(frequBoot), col = 1, lwd = 2)
      abline(v = outliers, col = "red", lwd = 2)

      # legend("center", c(paste("p=", round(out$p.value, digits = 5)), paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(out$p.value < 0.05, "red", "black" ))
    }

    if(plot == T) {

      hist(simulationOutput, main = "")

      main = ifelse(out$p.value <= 0.05,
                    "Outlier test significant",
                    "Outlier test n.s.")

      title(main = main, cex.main = 1,
            col.main = ifelse(out$p.value <= 0.05, "red", "black"))

    }
  }
  return(out)
}



#' Test for categorical dependencies
#'
#' This function tests if there are probles in a res ~ group structure. It performs two tests: test for within-group uniformity, and test for between-group homogeneity of variances
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param catPred a categorical predictor with the same dimensions as the residuals in simulationOutput
#' @param quantiles whether to draw the quantile lines.
#' @param plot if TRUE, the function will create an additional plot
#' @details The function tests for two common problems: are residuals within each group distributed according to model assumptions, and is the variance between group heterogeneous.
#'
#'  The test for within-group uniformity is performed via multipe KS-tests, with adjustment of p-values for multiple testing. If the plot is drawn, problematic groups are highlighted in red, and a corresponding message is displayed in the plot.
#'
#'  The test for homogeneity of variances is done with a Levene test. A significant p-value means that group variances are not constant. In this case, you should consider modelling variances, e.g. via ~dispformula in glmmTMB.
#'
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @example inst/examples/testsHelp.R
#' @export
testCategorical <- function(simulationOutput, catPred,
                            quantiles = c(0.25, 0.5, 0.75), plot = TRUE){

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  catPred = as.factor(catPred)
  out = list()

  out$uniformity$details = suppressWarnings(by(simulationOutput$scaledResiduals,
                                               catPred, ks.test, 'punif',
                                               simplify = TRUE))
  out$uniformity$p.value = rep(NA, nlevels(catPred))
  for(i in 1:nlevels(catPred)) out$uniformity$p.value[i] = out$uniformity$details[[i]]$p.value
  out$uniformity$p.value.cor = p.adjust(out$uniformity$p.value)

  if(nlevels(catPred) > 1) out$homogeneity = leveneTest_formula(simulationOutput$scaledResiduals ~ catPred)

  if(plot == T){
    boxplot(simulationOutput$scaledResiduals ~ catPred, ylim = c(0,1), axes = FALSE, col = ifelse(out$uniformity$p.value.cor < 0.05, "red", "lightgrey"))
    axis(1, at = 1:nlevels(catPred), levels(catPred))
    axis(2, at=c(0, quantiles, 1))
    abline(h = quantiles, lty = 2)
  }

  mtext(ifelse(any(out$uniformity$p.value.cor < 0.05), "Within-group deviations from uniformity significant (red)", "Within-group deviation from uniformity n.s."),
        col = ifelse(any(out$uniformity$p.value.cor < 0.05), "red", "black"),
        line = 1)

  if(length(out) > 1) {
    mtext(ifelse(out$homogeneity$`Pr(>F)`[1] < 0.05, "Levene Test for homogeneity of variance significant", "Levene Test for homogeneity of variance n.s."),
          col = ifelse(out$homogeneity$`Pr(>F)`[1] < 0.05, "red", "black"))
  }

  return(out)

}


#' DHARMa dispersion tests
#'
#' This function performs simulation-based tests for over/underdispersion. If type = "DHARMa" (default and recommended), simulation-based dispersion tests are performed. Their behavior differs depending on whether simulations are done with refit = F, or refit = T, and whether data is simulated conditional (e.g. re.form ~0 in lme4) (see below). If type = "PearsonChisq", a chi2 test on Pearson residuals is performed.
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis. Greater corresponds to testing only for overdispersion. It is recommended to keep the default setting (testing for both over and underdispersion)
#' @param plot whether to provide a plot for the results
#' @param type which test to run. Default is DHARMa, other options are PearsonChisq (see details)
#' @param ... arguments to pass on to [testGeneric]
#'
#' @details Over / underdispersion means that the observed data is more / less dispersed than expected under the fitted model. There is no unique way to test for dispersion problems, and there are a number of different dispersion tests implemented in various R packages.
#'
#' The testDispersion function implements several dispersion tests:
#'
#' **Simulation-based dispersion tests (type == "DHARMa")**
#'
#' If type = "DHARMa" (default and recommended), simulation-based dispersion tests are performed. Their behavior differs depending on whether simulations are done with refit = F, or refit = T
#'
#' #' **Important:** for either refit = T or F, the results of type = "DHARMa" dispersion test will differ depending on whether simulations are done conditional (= conditional on fitted random effects) or unconditional (= REs are re-simulated). How to change between conditional or unconditional simulations is discussed in [simulateResiduals]. The general default in DHARMa is to use unconditional simulations, because this has advantages in other situations, but dispersion tests for models with strong REs specifically may increase substantially in power / sensitivity when switching to conditional simulations. I therefore recommend checking dispersion with conditional simulations if supported by the used regression package.
#'
#' If refit = F, the function uses [testGeneric] to compare the variance of the observed raw residuals (i.e. var(observed - predicted), displayed as a red line) against the variance of the simulated residuals (i.e. var(simulated - predicted), histogram). The variances are scaled to the mean simulated variance. A significant ratio > 1 indicates overdispersion, a significant ratio < 1 underdispersion.
#'
#' If refit = T, the function compares the approximate deviance (via squared pearson residuals) with the same quantity from the models refitted with simulated data. Applying this is much slower than the previous alternative. Given the computational cost, I would suggest that most users will be satisfied with the standard dispersion test.
#'
#' ** Analytical dispersion tests (type == "PearsonChisq")**
#'
#' This is the test described in https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion, identical to performance::check_overdispersion. Works only if the fitted model provides df.residual and Pearson residuals.
#'
#' The test statistics is biased to lower values under quite general conditions, and will therefore tend to test significant for underdispersion. It is recommended to use this test only for overdispersion, i.e. use alternative == "greater". Also, obviously, it requires that Pearson residuals are available for the chosen model, which will not be the case for all models / packages.
#'
#' @note For particular model classes / situations, there may be more powerful and thus preferable over the DHARMa test. The advantage of the DHARMa test is that it directly targets the spread of the data (unless other tests such as dispersion/df, which essentially measure fit and may thus be triggered by problems other than dispersion as well), and it makes practically no assumptions about the fitted model, other than the availability of simulations.
#'
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @example inst/examples/testDispersionHelp.R
#' @export
testDispersion <- function(simulationOutput, alternative = c("two.sided", "greater", "less"), plot = T, type = c("DHARMa", "PearsonChisq"), ...){

  alternative <- match.arg(alternative)
  type <- match.arg(type)

  out = list()
  out$data.name = deparse(substitute(simulationOutput))

  if(type == "DHARMa"){

    simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  # if(class(simulationOutput$fittedModel) %in% c("glmerMod", "lmerMod"){
  #   if(!"re.form" %in% names(simulationOutput$additionalParameters) & is.null(simulationOutput$additionalParameters$re.form)) message("recommended to run conditional simulations for dispersion test, see help")
  #}

  if(simulationOutput$refit == F){

      expectedVar = sd(simulationOutput$simulatedResponse)^2
      spread <- function(x) var(x - simulationOutput$fittedPredictedResponse) / expectedVar
      out = testGeneric(simulationOutput, summary = spread, alternative = alternative, methodName = "DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated", plot = plot, ...)
      names(out$statistic) = "dispersion"
    } else {

      observed = tryCatch(sum(residuals(simulationOutput$fittedModel, type = "pearson")^2), error = function(e) {
        message(paste("DHARMa: the requested tests requires pearson residuals, but your model does not implement these calculations. Test will return NA. Error message:", e))
        return(NA)
      })
      if(is.na(observed)) return(NA)
      expected = apply(simulationOutput$refittedPearsonResiduals^2 , 2, sum)
      out$statistic = c(dispersion = observed / mean(expected))
      names(out$statistic) = "dispersion"
      out$method = "DHARMa nonparametric dispersion test via mean deviance residual fitted vs. simulated-refitted"

      p = getP(simulated = expected, observed = observed, alternative = alternative)

      out$alternative = alternative
      out$p.value = p
      class(out) = "htest"

      if(plot == T) {
        #plotTitle = gsub('(.{1,50})(\\s|$)', '\\1\n', out$method)
        xLabel = paste("Simulated values, red line = fitted model. p-value (",out$alternative, ") = ", out$p.value, sep ="")

        hist(expected, xlim = range(expected, observed, na.rm=T ), col = "lightgrey", main = "", xlab = xLabel, breaks = 20, cex.main = 1)
        abline(v = observed, lwd= 2, col = "red")

        main = ifelse(out$p.value <= 0.05,
                      "Dispersion test significant",
                      "Dispersion test n.s.")

        title(main = main, cex.main = 1,
              col.main = ifelse(out$p.value <= 0.05, "red", "black"))
      }
    }

  } else if(type == "PearsonChisq"){

    if("DHARMa" %in% class(simulationOutput)){
      model = simulationOutput$fittedModel
    }
    else model = simulationOutput

    if(! alternative == "greater") message("Note that the chi2 test on Pearson residuals is biased for MIXED models towards underdispersion. Tests with alternative = two.sided or less are therefore not reliable. If you have random effects in your model, I recommend to test only with alternative = 'greater', i.e. test for overdispersion, or else use the DHARMa default tests which are unbiased. See help for details.")

    rdf <- df.residual(model)
    rp <- getPearsonResiduals(model)
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    if(alternative == "greater") pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
    else if (alternative == "less") pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=TRUE)
    else if (alternative == "two.sided") pval <- min(min(pchisq(Pearson.chisq, df=rdf, lower.tail=TRUE), pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)) * 2,1)

    out$statistic = prat
    names(out$statistic) = "dispersion"
    out$parameter = rdf
    names(out$parameter) = "df"
    out$method = "Parametric dispersion test via mean Pearson-chisq statistic"
    out$alternative = alternative
    out$p.value = pval
    class(out) = "htest"
    # c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
    return(out)
  }

  return(out)
}

#' Simulated overdisperstion tests (deprecated)
#'
#' @details Deprecated, switch your code to using the [testDispersion] function
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via [simulateResiduals] or by [createDHARMa] for simulations created outside DHARMa
#' @param ... additional arguments to [testDispersion]
#' @export
testOverdispersion <- function(simulationOutput, ...){
  message("testOverdispersion is deprecated, switch your code to using the testDispersion function")
  testDispersion(simulationOutput, ...)
}

#' Parametric overdisperstion tests (deprecated)
#'
#' @details Deprecated, switch your code to using the [testDispersion] function.
#'
#' @param ... arguments will be ignored, the parametric tests is no longer recommend
#' @export
testOverdispersionParametric <- function(...){
  message("testOverdispersionParametric is deprecated - switch your code to using the testDispersion function")
  return(0)
}




#' Tests for zero-inflation
#'
#' This function compares the observed number of zeros with the zeros expected from simulations.
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param ... further arguments to [testGeneric]
#' @details Zero-inflation means that the observed data contain more zeros than would be expected under the fitted model. Zero-inflation must always be accessed with respect to a particular model, so the mere fact that there are many zeros in the observed data is not an indication of zero-inflation, see Warton, D. I. (2005). Many zeros does not mean zero inflation: comparing the goodness-of-fit of parametric models to multivariate abundance data. Environmetrics 16(3), 275-289.
#'
#' The testZeroInflation function simulates new datasets from the fitted model and compares this null distribution (gray histogram in the plot) with the observed values (red line in the plot). Technically, it is a wrapper for [testGeneric], with the summary argument set to function(x) sum(x == 0). The test statistic is the ratio of observed to simulated zeros. A value < 1 means that the observed data have fewer zeros than expected, a value > 1 means that they have more zeros than expected (aka zero inflation). By default, the function tests both sides, so it would also test for fewer zeros than expected.
#'
#' @note Zero-inflation can occur for a number of reasons other than an underlying data generating process corresponding to a ZIP model. Vice versa, it is very well possible that no zero-inflation will be observed when fitting models to data derived from a ZIP process. The latter is due to the fact that excess zeros can often be explained by other model parameters, such as the theta parameter in the negative binomial.
#'
#' For this reason, results of the zero-inflation test should be interpreted as a residual pattern that can have many reasons, not as a decision criterion for whether or not to fit a ZIP model. To decide whether to add a ZIP term, I would advise relying on appropriate model selection techniques such as AIC, BIC, WAIC, Bayes factor, or LRT. Note that these tests are often not reliable in GLMMs because it is difficult to determine the df spent by the different models. The [simulateLRT] function in DHARMa provides a nonparametric alternative to obtain p-values for LRT is nested models with unknown df.
#'
#' @author Florian Hartig
#' @example inst/examples/testsHelp.R
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @export
testZeroInflation <- function(simulationOutput, ...){
  countZeros <- function(x) sum( x == 0)
  testGeneric(simulationOutput = simulationOutput, summary = countZeros, methodName = "DHARMa zero-inflation test via comparison to expected zeros with simulation under H0 = fitted model", ... )
}


#' Test for a generic summary statistic based on simulated data
#'
#' This function tests if a user-defined summary differs when applied to simulated / observed data.
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param summary a function that can be applied to simulated / observed data. See examples below
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis
#' @param plot whether to plot the simulated summary
#' @param methodName name of the test (will be used in plot)
#'
#' @details This function applies a user-defined summary to the simulated/observed data of a DHARMa object and then performs a hypothesis test using the ratio Obs / Sim as the test statistic.
#'
#' The summary is applied directly to the data and not to the residuals, but it can easily be remodeled to apply summaries to the residuals by simply defining something like f = function(x) summary (x - predictions), as done in [testDispersion]
#'
#' @note The summary function you specify will be applied to the data as it appears in your fitted model, which may not always be what you want.
#'
#' As an example, consider the case where we want to test for n-inflation in k/n data. If you provide your data via cbind (k, n-k), you have to test for n-inflation, but if you provide your data via k/n and weights = n, you should test for 1-inflation. When in doubt, check how the data is represented internally in model.frame(model) or via simulate(model).
#'
#' @export
#' @author Florian Hartig
#' @example inst/examples/testsHelp.R
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
testGeneric <- function(simulationOutput, summary, alternative = c("two.sided", "greater", "less"), plot = T, methodName = "DHARMa generic simulation test"){

  out = list()
  out$data.name = deparse(substitute(simulationOutput))

  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  alternative <- match.arg(alternative)

  observed = summary(simulationOutput$observedResponse)

  simulated = apply(simulationOutput$simulatedResponse, 2, summary)

  p = getP(simulated = simulated, observed = observed, alternative = alternative)

  out$statistic = c(ratioObsSim = observed / mean(simulated))
  out$method = methodName
  out$alternative = alternative
  out$p.value = p


  class(out) = "htest"

  if(plot == T) {
    plotTitle = gsub('(.{1,50})(\\s|$)', '\\1\n', methodName)
    xLabel = paste("Simulated values, red line = fitted model. p-value (",out$alternative, ") = ", out$p.value, sep ="")
   hist(simulated, xlim = range(simulated, observed, na.rm=T ), col = "lightgrey", main = plotTitle, xlab = xLabel, breaks = max(round(simulationOutput$nSim / 5), 20), cex.main = 0.8)
   abline(v = observed, lwd= 2, col = "red")
  }
  return(out)
}


#' Test for temporal autocorrelation
#'
#' This function performs a standard test for temporal autocorrelation on the simulated residuals
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or by [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param time the time, in the same order as the data points.
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis
#' @param plot whether to plot output
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#'
#' Testing for temporal autocorrelation requires unique time values - if you have several observations per time value, either use [recalculateResiduals] function to aggregate residuals per time step, or extract the residuals from the fitted object, and plot / test each of them independently for temporally repeated subgroups (typical choices would be location / subject etc.). Note that the latter must be done by hand, outside testTemporalAutocorrelation.
#'
#' @note Standard DHARMa simulations from models with (temporal / spatial / phylogenetic) conditional autoregressive terms will still have the respective temporal / spatial / phylogenetic correlation in the DHARMa residuals, unless the package you are using is modelling the autoregressive terms as explicit REs and is able to simulate conditional on the fitted REs. This has two consequences
#'
#' 1. If you check the residuals for such a model, they will still show significant autocorrelation, even if the model fully accounts for this structure.
#'
#' 2. Because the DHARMa residuals for such a model are not statistically independent any more, other tests (e.g. dispersion, uniformity) may have inflated type I error, i.e. you will have a higher likelihood of spurious residual problems.
#'
#' There are three (non-exclusive) routes to address these issues when working with spatial / temporal / other autoregressive models:
#'
#' 1. Simulate conditional on the fitted CAR structures (see conditional simulations in the help of [simulateResiduals])
#'
#' 2. Rotate simulations prior to residual calculations (see parameter rotation in [simulateResiduals])
#'
#' 3. Use custom tests / plots that explicitly compare the correlation structure in the simulated data to the correlation structure in the observed data.
#'
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @example inst/examples/testTemporalAutocorrelationHelp.R
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time, alternative = c("two.sided", "greater", "less"), plot = TRUE){

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  # actually not sure if this is neccessary for dwtest, but seems better to aggregate
  if(any(duplicated(time))) stop("testing for temporal autocorrelation requires unique time values - if you have several observations per time value, either use the recalculateResiduals function to aggregate residuals per time step, or extract the residuals from the fitted object, and plot / test each of them independently for temporally repeated subgroups (typical choices would be location / subject etc.). Note that the latter must be done by hand, outside testTemporalAutocorrelation.")

  alternative <- match.arg(alternative)

  if(is.null(time)){
    time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
    message("DHARMa::testTemporalAutocorrelation - no time argument provided, using random times for each data point")
  }

  # To avoid Issue #190
  if (length(time) != length(residuals(simulationOutput))) stop("Dimensions of time don't match the dimension of the residuals")

  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time, alternative = alternative)

  if(plot == T) {
    oldpar <- par(mfrow = c(1,2))
    on.exit(par(oldpar))

    plot(simulationOutput$scaledResiduals[order(time)] ~ time[order(time)],
         type = "l", ylab = "Scaled residuals", xlab = "Time", main = "Residuals vs. time", ylim = c(0,1))

    abline(h=c(0.5))
    abline(h=c(0,0.25,0.75,1), lty = 2 )

    acf(simulationOutput$scaledResiduals[order(time)], main = "Autocorrelation", ylim = c(-1,1))
    legend("topright",
           c(paste(out$method, " p=", round(out$p.value, digits = 5)),
             paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))),
           text.col = ifelse(out$p.value < 0.05, "red", "black" ), bty="n")

  }

  return(out)
}


#' Test for distance-based spatial (or similar type) autocorrelation
#'
#' This function performs a Moran's I test for distance-based spatial (or similar type) autocorrelation on the calculated quantile residuals.
#'
#' @param simulationOutput An object of class DHARMa, either created via [simulateResiduals] for supported models or via [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param x The x coordinate, in the same order as the data points. Must be specified unless distMat is provided.
#' @param y The y coordinate, in the same order as the data points. Must be specified unless distMat is provided.
#' @param distMat Optional distance matrix. If not provided, euclidean distances based on x and y will be calculated. See details for explanation.
#' @param alternative A character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis.
#' @param plot If T, and if x and y is provided, plot the output (see Details).
#'
#' @details The function performs Moran.I test from the package ape on the DHARMa residuals. If a distance matrix (distMat) is provided, calculations will be based on this distance matrix, and x,y coordinates will only used for the plotting (if provided). If distMat is not provided, the function will calculate the euclidean distances between x,y coordinates, and test Moran.I based on these distances.
#'
#' If plot = T, a plot will be produced showing each residual with at its x,y position, colored according to the residual value. Residuals with 0.5 are colored white, everything below 0.5 is colored increasinly red, everything above 0.5 is colored increasingly blue.
#'
#' Testing for spatial autocorrelation requires unique x,y values - if you have several observations per location, either use the [recalculateResiduals] function to aggregate residuals per location, or extract the residuals from the fitted object, and plot / test each of them independently for spatially repeated subgroups (a typical scenario would repeated spatial observation, in which case one could plot / test each time step separately for temporal autocorrelation). Note that the latter must be done by hand, outside [testSpatialAutocorrelation].
#'
#' @note Standard DHARMa simulations from models with (temporal / spatial / phylogenetic) conditional autoregressive terms will still have the respective temporal / spatial / phylogenetic correlation in the DHARMa residuals, unless the package you are using is modelling the autoregressive terms as explicit REs and is able to simulate conditional on the fitted REs. This has two consequences:
#'
#' 1. If you check the residuals for such a model, they will still show significant autocorrelation, even if the model fully accounts for this structure.
#'
#' 2. Because the DHARMa residuals for such a model are not statistically independent any more, other tests (e.g. dispersion, uniformity) may have inflated type I error, i.e. you will have a higher likelihood of spurious residual problems.
#'
#' There are three (non-exclusive) routes to address these issues when working with spatial / temporal / phylogenetic / other autoregressive models:
#'
#' 1. Simulate conditional on the fitted CAR structures (see conditional simulations in the help of [simulateResiduals]).
#'
#' 2. Rotate simulations prior to residual calculations (see parameter rotation in [simulateResiduals]).
#'
#' 3. Use custom tests / plots that explicitly compare the correlation structure in the simulated data to the correlation structure in the observed data.
#'
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @import grDevices
#' @example inst/examples/testSpatialAutocorrelationHelp.R
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x = NULL, y  = NULL, distMat = NULL, alternative = c("two.sided", "greater", "less"), plot = TRUE){

  alternative <- match.arg(alternative)
  data.name = deparse(substitute(simulationOutput)) # needs to be before ensureDHARMa
  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  # Assertions

  if(any(duplicated(cbind(x,y)))) stop("Testing for spatial autocorrelation requires unique x,y values - if you have several observations per location, either use the recalculateResiduals function to aggregate residuals per location, or extract the residuals from the fitted object, and plot / test each of them independently for spatially repeated subgroups (a typical scenario would repeated spatial observation, in which case one could plot / test each time step separately for temporal autocorrelation). Note that the latter must be done by hand, outside testSpatialAutocorrelation.")

  if( (!is.null(x) | !is.null(y)) & !is.null(distMat) ) message("Both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will only be used for plotting.")

  if( (is.null(x) | is.null(y)) & is.null(distMat) ) stop("You need to provide either x,y, coordinates or a distMatrix.")

  if(is.null(distMat) & (length(x) != length(residuals(simulationOutput)) | length(y) != length(residuals(simulationOutput))))

  # To avoid Issue #190
  if (!is.null(x) & length(x) != length(residuals(simulationOutput)) | !is.null(y) & length(y) != length(residuals(simulationOutput))) stop("Dimensions of x / y coordinates don't match the dimension of the residuals.")

  # if not provided, create distance matrix based on x and y
  if(is.null(distMat)) distMat <- as.matrix(dist(cbind(x, y)))

  invDistMat <- 1/distMat
  diag(invDistMat) <- 0

  MI = ape::Moran.I(simulationOutput$scaledResiduals, weight = invDistMat, alternative = alternative)

  out = list()
  out$statistic = c(observed = MI$observed, expected = MI$expected, sd = MI$sd)
  out$method = "DHARMa Moran's I test for distance-based autocorrelation"
  out$alternative = "Distance-based autocorrelation"
  out$p.value = MI$p.value
  out$data.name = data.name

  class(out) = "htest"

  if(plot == T & !is.null(x) & !is.null(y)) {
    opar <- par(mfrow = c(1,1))
    on.exit(par(opar))

    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    plot(x,y, col = rgb(col, maxColorValue = 255), main = out$method, cex.main = 0.8 )

    # TODO implement correlogram
  }

  return(out)
}


#' Test for phylogenetic autocorrelation
#'
#' This function performs a Moran's I test for phylogenetic autocorrelation on the calculated quantile residuals.
#'
#' @param simulationOutput an object of class DHARMa, either created via [simulateResiduals] for supported models or via [createDHARMa] for simulations created outside DHARMa, or a supported model. Providing a supported model directly is discouraged, because simulation settings cannot be changed in this case.
#' @param tree A phylogenetic tree object.
#' @param alternative A character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis of no phylogenetic correlation.
#'
#' @details The function performs Moran.I test from the package ape on the DHARMa residuals, based on the phylogenetic distance matrix internally created from the provided tree. For custom distance matrices, you can use [testSpatialAutocorrelation].
#'
#' @note Standard DHARMa simulations from models with (temporal / spatial / phylogenetic) conditional autoregressive terms will still have the respective temporal / spatial / phylogenetic correlation in the DHARMa residuals, unless the package you are using is modelling the autoregressive terms as explicit REs and is able to simulate conditional on the fitted REs. This has two consequences:
#'
#' 1. If you check the residuals for such a model, they will still show significant autocorrelation, even if the model fully accounts for this structure.
#'
#' 2. Because the DHARMa residuals for such a model are not statistically independent any more, other tests (e.g. dispersion, uniformity) may have inflated type I error, i.e. you will have a higher likelihood of spurious residual problems.
#'
#' There are three (non-exclusive) routes to address these issues when working with spatial / temporal / phylogenetic autoregressive models:
#'
#' 1. Simulate conditional on the fitted CAR structures (see conditional simulations in the help of [simulateResiduals]).
#'
#' 2. Rotate simulations prior to residual calculations (see parameter rotation in [simulateResiduals]).
#'
#' 3. Use custom tests / plots that explicitly compare the correlation structure in the simulated data to the correlation structure in the observed data.
#'
#' @author Florian Hartig
#' @seealso [testResiduals], [testUniformity], [testOutliers], [testDispersion], [testZeroInflation], [testGeneric], [testTemporalAutocorrelation], [testSpatialAutocorrelation], [testQuantiles], [testCategorical]
#' @example inst/examples/testPhylogeneticAutocorrelationHelp.R
#' @export
testPhylogeneticAutocorrelation <- function(simulationOutput,
                                            tree,
                                            alternative = c("two.sided", "greater", "less")){

  alternative <- match.arg(alternative)
  data.name = deparse(substitute(simulationOutput)) # needs to be before ensureDHARMa
  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  # calculate distance matrix
  distMat <- cophenetic(tree)
  invDistMat <- 1/distMat
  diag(invDistMat) <- 0

  MI = ape::Moran.I(simulationOutput$scaledResiduals, weight = invDistMat, alternative = alternative)

  out = list()
  out$statistic = c(observed = MI$observed, expected = MI$expected, sd = MI$sd)
  out$method = "DHARMa Moran's I test for phylogenetic autocorrelation"
  out$alternative = "Phylogenetic autocorrelation"
  out$p.value = MI$p.value
  out$data.name = data.name

  class(out) = "htest"
  return(out)
}



getP <- function(simulated, observed, alternative, plot = FALSE, ...){

  if(alternative == "greater") p = mean(simulated >= observed)
  if(alternative == "less") p = mean(simulated <= observed)
  if(alternative == "two.sided") p = min(min(mean(simulated <= observed), mean(simulated >= observed) ) * 2,1)

  if(plot == T){
    hist(simulated, xlim = range(simulated, observed), col = "lightgrey", main = "Distribution of test statistic \n grey = simulated, red = observed", ...)
    abline(v = mean(simulated), col = 1, lwd = 2)
    abline(v = observed, col = "red", lwd = 2)
  }

  return(p)
}
