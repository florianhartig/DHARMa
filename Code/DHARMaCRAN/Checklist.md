Submission Checklist

https://cran.r-project.org/web/checks/check_results_DHARMa.html


## Prepare release

* All issues resolved?
  * Appropriate unit tests created?
  * Update help / description files -> date, version number, etc.
* Make sure RStudio check function checks out
* Cross-check compatibility with CRAN requirements

## Test release

* Make sure that help files / tests don't take too long (5s on slow computer max for CRAN)
* Check TravisCI automatic tests
* Check package in http://win-builder.r-project.org/upload.aspx
* Check Reverse dependencies
  * https://github.com/yihui/crandalf
  * devtools::revdep()



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






