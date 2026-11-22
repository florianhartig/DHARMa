# tests work

    list(statistic = 0.046404995137744, p.value = 0.782244345306786)

---

    list(statistic = 0.0303172078924587, p.value = 0.692357223410798)

---

    list(statistic = 0.046404995137744, p.value = 0.422582988286365)

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

    list(statistic = 0.046404995137744, p.value = 0.782244345306786)

---

    list(statistic = 1.24710202622324, p.value = 0.08)

---

    list(statistic = 2, p.value = 1)

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

    list(statistic = 0.24236823311391, p.value = 0.523144542137211)

---

    list(statistic = 0.130838754741972, p.value = 0.65576829763515)

---

    list(statistic = 0.24236823311391, p.value = 0.265653558162143)

---

    list(statistic = 2.14164584585957, p.value = 0.064)

---

    list(statistic = 2.14164584585957, p.value = 0.968)

---

    list(statistic = 2.14164584585957, p.value = 0.032)

---

    list(statistic = 2.14164584585957, p.value = 0.064)

---

    list(statistic = 0.24236823311391, p.value = 0.523144542137211)

---

    list(statistic = 2.14164584585957, p.value = 0.064)

---

    list(statistic = 1, p.value = 0.26)

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
      observed = -0.0185821, expected = -0.0050251, sd = 0.0112745, p-value =
      0.2292
      alternative hypothesis: two.sided
      

---

    list(statistic = c(-0.0185820861732069, -0.0050251256281407, 
    0.0112744972438508), p.value = 0.229191242002184)

---

    list(statistic = c(-0.0185820861732069, -0.0050251256281407, 
    0.0112744972438508), p.value = 0.229191242002184)

---

    list(statistic = c(-0.0185820861732069, -0.0050251256281407, 
    0.0112744972438508), p.value = 0.229191242002184)

---

    list(statistic = c(-0.0185820861732069, -0.0050251256281407, 
    0.0112744972438508), p.value = 0.229191242002184)

---

    list(statistic = c(-0.0185820861732069, -0.0050251256281407, 
    0.0112744972438508), p.value = 0.229191242002184)

---

    list(statistic = 2.03762175906022, p.value = 0.789189794363778)

---

    list(statistic = 2.03762175906022, p.value = 0.605405102818111)

# test phylogenetic autocorrelation

    Code
      restest2
    Output
      
      	DHARMa Moran's I test for phylogenetic autocorrelation
      
      data:  res2
      observed = -0.0077989, expected = -0.0169492, sd = 0.0883326, p-value =
      0.9175
      alternative hypothesis: two.sided
      

# testOutliers

    Code
      testOutliers(simulationOutput, plot = F, margin = "lower")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at lower margin(s) = 3, observations = 1000, p-value = 1
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.0006190999 0.0087420232
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.003 
      

---

    list(statistic = 3, p.value = 1)

---

    list(statistic = 4, p.value = 0.803658572542606)

