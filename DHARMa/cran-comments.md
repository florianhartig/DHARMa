
# Version 0.1.0

## Tested under

* local MAC OS 10.10.5, R 3.3.1
* local Windows 2012 Server, R 3.3.0
* http://win-builder.r-project.org/
 * R-release: R-release, currently R-3.3.1 
 * R-devel: R-devel, to be R-3.4.0

## R CMD check results

#### local

R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded

#### win-builder

Notes:

Possibly mis-spelled words in DESCRIPTION:
  DHARMa (7:18)
  HierArchical (2:33)
  Multi (2:47)
  autocorrelation (14:5)
  interpretable (8:13)
  misspecification (12:73)
  underdispersion (13:27)

The Title field should be in title case, current version then in title case:
'Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models'
'Residual Diagnostics for HierArchical (Multi-Level / Mixed) Regression Models'

--> This seems fine to me. AFAIKS, the spelling is as intendend, and the title is in title case
