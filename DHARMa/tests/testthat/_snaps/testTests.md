# tests work

    list(statistic = 0.0355771289588884, p.value = 0.961903549025525)

---

    list(statistic = 0.0355771289588884, p.value = 0.602725842226698)

---

    list(statistic = 0.0344183805584907, p.value = 0.622600655515334)

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

    list(statistic = 1.2577808560684, p.value = 0.952)

---

    list(statistic = 1.2577808560684, p.value = 0.048)

---

    list(statistic = 1.2577808560684, p.value = 0.096)

---

    list(statistic = 0.0355771289588884, p.value = 0.961903549025525)

---

    list(statistic = 1.2577808560684, p.value = 0.096)

---

    list(statistic = 1, p.value = 0.82)

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

    list(statistic = 1.05652403592182, p.value = 0.784)

---

    list(statistic = 0.974565439672802, p.value = 0.808)

---

    list(statistic = 0.974565439672802, p.value = 0.404)

---

    Code
      testGeneric(simulationOutput, summary = means, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0077, p-value = 0.944
      alternative hypothesis: two.sided
      

---

    list(statistic = 1.08340110407644, p.value = 0.208)

---

    list(statistic = 0.225467124093324, p.value = 0.613005306213096)

---

    list(statistic = 0.162786953665316, p.value = 0.533775758500823)

---

    list(statistic = 0.225467124093324, p.value = 0.315000480341144)

---

    list(statistic = 2.10913188260749, p.value = 0.048)

---

    list(statistic = 2.10913188260749, p.value = 0.976)

---

    list(statistic = 2.10913188260749, p.value = 0.024)

---

    list(statistic = 2.10913188260749, p.value = 0.048)

---

    list(statistic = 0.225467124093324, p.value = 0.613005306213096)

---

    list(statistic = 2.10913188260749, p.value = 0.048)

---

    list(statistic = 1, p.value = 0.1)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = 1.00766772156881, p.value = 0.944)

---

    list(statistic = 1.47251978475856, p.value = 0.04)

---

    list(statistic = 1.12305439409853, p.value = 0.248)

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

    list(statistic = c(-0.0166319446603349, -0.0050251256281407, 
    0.011274982411173), p.value = 0.303277079229004)

---

    list(statistic = c(-0.0166319446603349, -0.0050251256281407, 
    0.011274982411173), p.value = 0.303277079229004)

---

    list(statistic = c(-0.0166319446603349, -0.0050251256281407, 
    0.011274982411173), p.value = 0.303277079229004)

---

    list(statistic = c(-0.0166319446603349, -0.0050251256281407, 
    0.011274982411173), p.value = 0.303277079229004)

---

    list(statistic = c(-0.0166319446603349, -0.0050251256281407, 
    0.011274982411173), p.value = 0.303277079229004)

---

    list(statistic = 1.97032354353526, p.value = 0.832966274638992)

---

    list(statistic = 1.97032354353526, p.value = 0.416483137319496)

# test phylogenetic autocorrelation

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

    list(statistic = 4, p.value = 0.803658572542606)

---

    list(statistic = 4, p.value = 0.803658572542606)

