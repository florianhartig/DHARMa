Submission Checklist

## Prepare release

* Check current CRAN issues at https://cran.r-project.org/web/checks/check_results_DHARMa.html
* All GitHub issues for the release resolved?
  * All new features / changes have help files, examples and appropriate unit tests 
  * Is the NEWS.md file updated?
  * Are all documentation files, including vignettes, updated
* Make sure RStudio check / test function produce no errors
* Make sure CI via GH Actions produces no errors
* Update description files -> date, version number, etc.

## Test release

* Make sure that help files / tests don't take too long (5s on slow computer max for CRAN)
* Check again GH ations
* Check package in http://win-builder.r-project.org/upload.aspx
  * Results go to maintainer
* Check Reverse dependencies
  * https://github.com/yihui/crandalf
  * devtools::revdep(), see code below
  
## CRAN upload

* Upload release to CRAN 
  * Can be done by anyone but upload confirmation goes maintainer
* Document upload text in DHARMa/cran-comments.md

## Package accepted

* Create release / tag on GH



### Code for reverse dependency checks (may need to be updated)

```{r}
revdep_maintainers

res <- devtools::revdep_check()
devtools::revdep_check_summary(res)
devtools::revdep_check_save_logs(res)

install.packages("rhub")
rhub::platforms()
rhub::check_for_cran("")

# check when a function was introduced to base
newsDB <- news()
print(news(grepl("startsWith",Text), db=newsDB), doBrowse=FALSE)
```






