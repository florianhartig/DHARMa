# tests work

    Code
      testUniformity(simulationOutput, plot = FALSE)
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.035577, p-value = 0.9619
      alternative hypothesis: two-sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^- = 0.035577, p-value = 0.6027
      alternative hypothesis: the CDF of x lies below the null hypothesis
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^+ = 0.034418, p-value = 0.6226
      alternative hypothesis: the CDF of x lies above the null hypothesis
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.2578, p-value = 0.096
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.2578, p-value = 0.952
      alternative hypothesis: less
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.2578, p-value = 0.048
      alternative hypothesis: greater
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.2578, p-value = 0.096
      alternative hypothesis: two.sided
      

---

    Code
      testResiduals(simulationOutput, plot = FALSE)
    Output
      $uniformity
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.035577, p-value = 0.9619
      alternative hypothesis: two-sided
      
      
      $dispersion
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.2578, p-value = 0.096
      alternative hypothesis: two.sided
      
      
      $outliers
      
      	DHARMa bootstrapped outlier test
      
      data:  simulationOutput
      outliers at both margin(s) = 1, observations = 200, p-value = 0.82
      alternative hypothesis: two.sided
       percent confidence interval:
       0.000000 0.012625
      sample estimates:
      outlier frequency (expected: 0.0028 ) 
                                      0.005 
      
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = 1.0565, p-value = 0.552
      alternative hypothesis: two.sided
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = 1.0565, p-value = 0.784
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 0.97457, p-value = 0.808
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 0.97457, p-value = 0.404
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = means, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0077, p-value = 0.944
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = spread, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0834, p-value = 0.208
      alternative hypothesis: two.sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE)
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.22547, p-value = 0.613
      alternative hypothesis: two-sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^- = 0.16279, p-value = 0.5338
      alternative hypothesis: the CDF of x lies below the null hypothesis
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^+ = 0.22547, p-value = 0.315
      alternative hypothesis: the CDF of x lies above the null hypothesis
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 2.1091, p-value = 0.048
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 2.1091, p-value = 0.976
      alternative hypothesis: less
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 2.1091, p-value = 0.024
      alternative hypothesis: greater
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 2.1091, p-value = 0.048
      alternative hypothesis: two.sided
      

---

    Code
      testResiduals(simulationOutput, plot = FALSE)
    Output
      $uniformity
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.22547, p-value = 0.613
      alternative hypothesis: two-sided
      
      
      $dispersion
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 2.1091, p-value = 0.048
      alternative hypothesis: two.sided
      
      
      $outliers
      
      	DHARMa bootstrapped outlier test
      
      data:  simulationOutput
      outliers at both margin(s) = 20, observations = 200, p-value = 0.1
      alternative hypothesis: two.sided
       percent confidence interval:
       0.0 0.1
      sample estimates:
      outlier frequency (expected: 0.005 ) 
                                       0.1 
      
      

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
      ratioObsSim = 1.0077, p-value = 0.944
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = spread, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.4725, p-value = 0.04
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via mean deviance residual fitted
      	vs. simulated-refitted
      
      data:  simulationOutput
      dispersion = 1.1231, p-value = 0.248
      alternative hypothesis: two.sided
      

# correlation tests work

    Code
      testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y,
      plot = FALSE)
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0166319, expected = -0.0050251, sd = 0.0112750, p-value =
      0.3033
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y,
      plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0166319, expected = -0.0050251, sd = 0.0112750, p-value =
      0.3033
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE)
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0166319, expected = -0.0050251, sd = 0.0112750, p-value =
      0.3033
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE,
        alternative = "two.sided")
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0166319, expected = -0.0050251, sd = 0.0112750, p-value =
      0.3033
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, plot = FALSE, x = testData$x[1:10],
      y = testData$y[1:9])
    Condition
      Error in `testSpatialAutocorrelation()`:
      ! Dimensions of x / y coordinates don't match the dimension of the residuals. Remove rows with NAs or specify x,y as formula to handle NAs automatically.

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
      Both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will be ignored.
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0166319, expected = -0.0050251, sd = 0.0112750, p-value =
      0.3033
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE, y = testData$
        y)
    Message
      Both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will be ignored.
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0166319, expected = -0.0050251, sd = 0.0112750, p-value =
      0.3033
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testTemporalAutocorrelation(simulationOutput, plot = FALSE, time = testData$
      time)
    Output
      
      	Durbin-Watson test
      
      data:  simulationOutput$scaledResiduals ~ 1
      DW = 1.9703, p-value = 0.833
      alternative hypothesis: true autocorrelation is not 0
      

---

    Code
      testTemporalAutocorrelation(simulationOutput, plot = FALSE, time = testData$
      time, alternative = "greater")
    Output
      
      	Durbin-Watson test
      
      data:  simulationOutput$scaledResiduals ~ 1
      DW = 1.9703, p-value = 0.4165
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
      observed = 0.047474, expected = -0.016949, sd = 0.088260, p-value =
      0.4654
      alternative hypothesis: Phylogenetic autocorrelation
      

# testOutliers

    Code
      testOutliers(simulationOutput, plot = F, margin = "lower")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at lower margin(s) = 4, observations = 1000, p-value = 0.8037
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.001090908 0.010209665
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.004 
      

---

    Code
      testOutliers(simulationOutput, plot = F, alternative = "two.sided", margin = "lower")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at lower margin(s) = 4, observations = 1000, p-value = 0.8037
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.001090908 0.010209665
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.004 
      

---

    Code
      testOutliers(simulationOutput, plot = F, margin = "upper")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at upper margin(s) = 4, observations = 1000, p-value = 0.8037
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.001090908 0.010209665
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.004 
      

