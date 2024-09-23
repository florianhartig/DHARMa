# tests work

    Code
      testUniformity(simulationOutput, plot = FALSE)
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.046573, p-value = 0.7785
      alternative hypothesis: two-sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^- = 0.027964, p-value = 0.7314
      alternative hypothesis: the CDF of x lies below the null hypothesis
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^+ = 0.046573, p-value = 0.4199
      alternative hypothesis: the CDF of x lies above the null hypothesis
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4873, p-value < 2.2e-16
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4873, p-value = 1
      alternative hypothesis: less
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4873, p-value < 2.2e-16
      alternative hypothesis: greater
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4873, p-value < 2.2e-16
      alternative hypothesis: two.sided
      

---

    Code
      testResiduals(simulationOutput, plot = FALSE)
    Output
      $uniformity
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.046573, p-value = 0.7785
      alternative hypothesis: two-sided
      
      
      $dispersion
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4873, p-value < 2.2e-16
      alternative hypothesis: two.sided
      
      
      $outliers
      
      	DHARMa bootstrapped outlier test
      
      data:  simulationOutput
      outliers at both margin(s) = 2, observations = 200, p-value = 0.26
      alternative hypothesis: two.sided
       percent confidence interval:
       0.00 0.01
      sample estimates:
      outlier frequency (expected: 0.0033 ) 
                                       0.01 
      
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = 0.98524, p-value = 0.96
      alternative hypothesis: two.sided
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = 0.98524, p-value = 0.48
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.1792, p-value = 0.104
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.1792, p-value = 0.972
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = means, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0072, p-value = 0.92
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = spread, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.1603, p-value < 2.2e-16
      alternative hypothesis: two.sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE)
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.19312, p-value = 0.7838
      alternative hypothesis: two-sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^- = 0.19312, p-value = 0.4204
      alternative hypothesis: the CDF of x lies below the null hypothesis
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^+ = 0.13112, p-value = 0.6547
      alternative hypothesis: the CDF of x lies above the null hypothesis
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4863, p-value = 0.248
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4863, p-value = 0.876
      alternative hypothesis: less
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4863, p-value = 0.124
      alternative hypothesis: greater
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4863, p-value = 0.248
      alternative hypothesis: two.sided
      

---

    Code
      testResiduals(simulationOutput, plot = FALSE)
    Output
      $uniformity
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.19312, p-value = 0.7838
      alternative hypothesis: two-sided
      
      
      $dispersion
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.4863, p-value = 0.248
      alternative hypothesis: two.sided
      
      
      $outliers
      
      	DHARMa bootstrapped outlier test
      
      data:  simulationOutput
      outliers at both margin(s) = 0, observations = 200, p-value = 1
      alternative hypothesis: two.sided
       percent confidence interval:
       0.0 0.1
      sample estimates:
      outlier frequency (expected: 0.007 ) 
                                         0 
      
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: two.sided
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = means, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0072, p-value = 0.92
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = spread, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 0.9245, p-value = 0.752
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via mean deviance residual fitted
      	vs. simulated-refitted
      
      data:  simulationOutput
      dispersion = 1.3813, p-value < 2.2e-16
      alternative hypothesis: two.sided
      

# correlation tests work

    Code
      testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y,
      plot = FALSE)
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0147107, expected = -0.0050251, sd = 0.0113003, p-value =
      0.3914
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y,
      plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0147107, expected = -0.0050251, sd = 0.0113003, p-value =
      0.3914
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE)
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0147107, expected = -0.0050251, sd = 0.0113003, p-value =
      0.3914
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE,
        alternative = "two.sided")
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0147107, expected = -0.0050251, sd = 0.0113003, p-value =
      0.3914
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, plot = FALSE, x = testData$x[1:10],
      y = testData$y[1:9])
    Condition
      Warning in `cbind()`:
      number of rows of result is not a multiple of vector length (arg 2)
      Error in `testSpatialAutocorrelation()`:
      ! Dimensions of x / y coordinates don't match the dimension of the residuals

---

    Code
      testSpatialAutocorrelation(simulationOutput[1:10], plot = FALSE, x = testData$x[
        1:10], y = testData$y[1:10])
    Condition
      Error in `ensureDHARMa()`:
      ! wrong argument to function, simulationOutput must be a DHARMa object or a numeric vector of quantile residuals!

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE, x = testData$
        x)
    Message
      both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will only be used for plotting
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0147107, expected = -0.0050251, sd = 0.0113003, p-value =
      0.3914
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE, y = testData$
        y)
    Message
      both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will only be used for plotting
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0147107, expected = -0.0050251, sd = 0.0113003, p-value =
      0.3914
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testTemporalAutocorrelation(simulationOutput, plot = FALSE, time = testData$
      time)
    Output
      
      	Durbin-Watson test
      
      data:  simulationOutput$scaledResiduals ~ 1
      DW = 2.1009, p-value = 0.4734
      alternative hypothesis: true autocorrelation is not 0
      

---

    Code
      testTemporalAutocorrelation(simulationOutput, plot = FALSE, time = testData$
      time, alternative = "greater")
    Output
      
      	Durbin-Watson test
      
      data:  simulationOutput$scaledResiduals ~ 1
      DW = 2.1009, p-value = 0.7633
      alternative hypothesis: true autocorrelation is greater than 0
      

# test phylogenetic autocorrelation

    Code
      restest
    Output
      
      	DHARMa Moran's I test for phylogenetic autocorrelation
      
      data:  res
      observed = 0.851667, expected = -0.016949, sd = 0.088733, p-value <
      2.2e-16
      alternative hypothesis: Phylogenetic autocorrelation
      

---

    Code
      restest2
    Output
      
      	DHARMa Moran's I test for phylogenetic autocorrelation
      
      data:  res2
      observed = -0.093598, expected = -0.016949, sd = 0.088473, p-value =
      0.3863
      alternative hypothesis: Phylogenetic autocorrelation
      

# testOutliers

    Code
      testOutliers(simulationOutput, plot = F, margin = "lower")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at lower margin(s) = 5, observations = 1000, p-value = 0.6079
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.00162542 0.01162947
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.005 
      

---

    Code
      testOutliers(simulationOutput, plot = F, alternative = "two.sided", margin = "lower")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at lower margin(s) = 5, observations = 1000, p-value = 0.6079
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.00162542 0.01162947
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.005 
      

---

    Code
      testOutliers(simulationOutput, plot = F, margin = "upper")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at upper margin(s) = 2, observations = 1000, p-value = 0.4519
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.0002423011 0.0072058389
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.002 
      

