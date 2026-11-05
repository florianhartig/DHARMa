# Code outside the package

Scripts and material that are not part of the package. Except for `DHARMaTests/` and `DHARMaCRAN/`, they are not updated when the package changes, so older scripts may not run with the current version.

| Folder | Content |
|---|---|
| `DHARMaCRAN/` | CRAN submission checklist |
| `DHARMaData/` | creation of the package dataset `hurricanes`, with raw data |
| `DHARMaDevelopment/` | prototypes and notes for new or changed functionality |
| `DHARMaExamples/` | worked examples of specific modelling problems |
| `DHARMaIssues/` | reprexes and simulations for GitHub issues |
| `DHARMaPackageSupport/` | checks of supported model classes, mostly one subfolder per package |
| `DHARMaPerformance/` | simulations of calibration and power of residuals and tests |
| `DHARMaTeaching/` | course and workshop material |
| `DHARMaTests/` | extended tests run with testthat, see the [README](DHARMaTests/README.md) there |
| `Visualization/` | script for a figure in the vignette |

## Adding files

Code for an issue (except tests) goes to `DHARMaIssues/<issue number>.R`, with data files under the same number (e.g., `392.R`, `392.csv`), and starts with a comment that says what it shows. Other code goes to the matching folder above. Older files do not all follow these rules.
