# tests work

    list(statistic = 0.0479322043191642, p.value = 0.747699126906491)

---

    list(statistic = 0.0260323977395892, p.value = 0.762559841572756)

---

    list(statistic = 0.0479322043191642, p.value = 0.398918364068668)

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.2471, p-value = 0.08
      alternative hypothesis: two.sided
      

---

    list(statistic = 1.24710202622324, p.value = 0.96)

---

    list(statistic = 1.24710202622324, p.value = 0.04)

---

    list(statistic = 1.24710202622324, p.value = 0.08)

---

    list(statistic = 0.0479322043191642, p.value = 0.747699126906491)

---

    list(statistic = 1.24710202622324, p.value = 0.08)

---

    list(statistic = 1, p.value = 0.98)

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = 1.0547, p-value = 0.656
      alternative hypothesis: two.sided
      

---

    list(statistic = 1.05472870033986, p.value = 0.752)

---

    list(statistic = 0.97096650961416, p.value = 0.848)

---

    list(statistic = 0.97096650961416, p.value = 0.424)

---

    Code
      testGeneric(simulationOutput, summary = means, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0071, p-value = 0.952
      alternative hypothesis: two.sided
      

---

    list(statistic = 1.07523793830879, p.value = 0.264)

---

    list(statistic = 0.240719932043925, p.value = 0.531731559712716)

---

    list(statistic = 0.132449687808752, p.value = 0.649774619838071)

---

    list(statistic = 0.240719932043925, p.value = 0.270264374492062)

---

    list(statistic = 2.14164584585957, p.value = 0.064)

---

    list(statistic = 2.14164584585957, p.value = 0.968)

---

    list(statistic = 2.14164584585957, p.value = 0.032)

---

    list(statistic = 2.14164584585957, p.value = 0.064)

---

    list(statistic = 0.240719932043925, p.value = 0.531731559712716)

---

    list(statistic = 2.14164584585957, p.value = 0.064)

---

    list(statistic = 1, p.value = 0.16)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = NaN, p.value = 1)

---

    list(statistic = 1.0070968858676, p.value = 0.952)

---

    list(statistic = 1.50571900974715, p.value = 0.024)

---

    list(statistic = 1.12200622650945, p.value = 0.2)

# correlation tests work

    Code
      testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y,
      plot = FALSE)
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = -0.0193785, expected = -0.0050251, sd = 0.0112743, p-value =
      0.203
      alternative hypothesis: two.sided
      

---

    list(statistic = c(-0.0193785030017313, -0.0050251256281407, 
    0.0112742680577377), p.value = 0.202979200795442)

---

    list(statistic = c(-0.0193785030017313, -0.0050251256281407, 
    0.0112742680577377), p.value = 0.202979200795442)

---

    list(statistic = c(-0.0193785030017313, -0.0050251256281407, 
    0.0112742680577377), p.value = 0.202979200795442)

---

    list(statistic = c(-0.0193785030017313, -0.0050251256281407, 
    0.0112742680577377), p.value = 0.202979200795442)

---

    list(statistic = c(-0.0193785030017313, -0.0050251256281407, 
    0.0112742680577377), p.value = 0.202979200795442)

---

    list(statistic = 2.02715548206735, p.value = 0.846972128892957)

---

    list(statistic = 2.02715548206735, p.value = 0.576513935553522)

# test phylogenetic autocorrelation

    Code
      restest2
    Output
      
      	DHARMa Moran's I test for phylogenetic autocorrelation
      
      data:  res2
      observed = -0.0080149, expected = -0.0169492, sd = 0.0883345, p-value =
      0.9194
      alternative hypothesis: two.sided
      

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

    list(statistic = 5, p.value = 0.607906169367581)

