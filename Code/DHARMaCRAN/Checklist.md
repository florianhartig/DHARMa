Submission Checklist

## Prepare release

* Check current CRAN issues at https://cran.r-project.org/web/checks/check_results_DHARMa.html
* All GitHub issues for the release resolved?
  * All new features / changes have help files, examples and appropriate unit tests? 
  * Is the NEWS.md file updated?
  * Are all documentation files, including README.MD, vignettes, updated?
* Make sure RStudio check / test function produce no errors
* Make sure CI via GH Actions produces no errors
* Update DESCRIPTION files -> date, version number, etc.

## Test release

* Make sure that help files / tests don't take too long (5s on slow computer max for CRAN)
* Check again GH ations
* Check package in http://win-builder.r-project.org/upload.aspx
  * Results go to maintainer
* Check Reverse dependencies
  * https://github.com/yihui/crandalf
  * [revdepcheck](https://github.com/r-lib/revdepcheck) package, see code below
  
## CRAN upload

* Upload release to CRAN 
  * Can be done by anyone but upload confirmation goes to maintainer
* Document upload text in DHARMa/cran-comments.md

## Package accepted

* Create release / tag on GH



### Code for reverse dependency checks

```{r}
# from R Packages book Chapeter 22

usethis::use_revdep() # changes .gitignore and .Rbuildignore 
revdepcheck::revdep_check(num_workers=4)

# it creates a folder revdep/ with the results of the checks
# cran.md is a high-level summary aimed at CRAN. Copy and paste it to cran-comments.md.

#OLD:
install.packages("rhub")
rhub::platforms()
rhub::check_for_cran("")

# check when a function was introduced to base
newsDB <- news()
print(news(grepl("startsWith",Text), db=newsDB), doBrowse=FALSE)
```






