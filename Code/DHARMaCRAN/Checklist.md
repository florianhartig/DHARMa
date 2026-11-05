# Submission Checklist

## Prepare release

* Check current CRAN issues at <https://cran.r-project.org/web/checks/check_results_DHARMa.html>
* All GitHub issues and pull requests in the release milestone resolved, or postponed to the next release?
  * All new features / changes have help files, examples and appropriate unit tests? 
  * Is the NEWS.md file updated?
  * Are all documentation files, including README.md, vignettes, updated?
* Make sure a local `R CMD check` produces no errors, warnings or notes
* Make sure CI via GH Actions (all OS) produces no errors, warnings or notes
* Check package in <https://win-builder.r-project.org/upload.aspx> (all R versions)
  * Results go to maintainer
* Update DESCRIPTION files -> date, version number, etc.
* Go through stale branches (GitHub branches page, "Stale" view = no commits in the last three months), except `master` and `archive/` branches: for each branch without an open pull request, open a draft pull request, or archive it with a tag and delete it (see code below)

## Test release

* Make sure that help files / tests don't take too long (5s on slow computer max for CRAN)
* Check again GH actions of the final files
* Check Reverse dependencies
  * GitHub Actions workflow "Reverse dependency check" (Actions tab, run manually)
  * <https://github.com/yihui/crandalf>
  * [revdepcheck](https://github.com/r-lib/revdepcheck) package, see code below
* Update DHARMa/cran-comments.md with text for submission
  
## CRAN upload

* Upload release to CRAN 
  * Can be done by anyone but upload confirmation goes to maintainer

## Package accepted

* Create release / tag on GH
* Set the version in DESCRIPTION to the next planned release and add a matching heading at the top of DHARMa/NEWS.md



### Code for archiving a stale branch

```bash
git fetch origin --prune
git tag archive/<branch> origin/<branch>
git push origin archive/<branch>
git push origin --delete refs/heads/<branch>
```

### Code for reverse dependency checks

```r
# from R Packages book Chapter 22

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






