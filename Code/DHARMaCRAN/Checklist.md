# Submission Checklist

Steps marked [maintainer] need the maintainer's e-mail; all other steps can be done by any team member.

## Prepare release

* Check reverse dependencies early (time for the 2-week notice below): GitHub Actions workflow "Reverse dependency check" (Actions tab, run manually)
  * If an intended change breaks packages: inform their maintainers with suggested changes at least 2 weeks before submitting
* Check current CRAN issues at <https://cran.r-project.org/web/checks/check_results_DHARMa.html>
* All GitHub issues and pull requests in the release milestone resolved, or postponed to the next release?
  * All new features / changes have help files, examples and appropriate unit tests? 
  * Is the NEWS.md file updated?
  * Are all documentation files, including README.md, vignettes, updated?
* Update DESCRIPTION files -> date, version number, etc.
* Make sure a local `R CMD check --as-cran` on the tarball from `R CMD build DHARMa` produces no errors, warnings or notes
  * `devtools::document("DHARMa")` leaves `git status` clean
* Go through stale branches (GitHub branches page, "Stale" view = no commits in the last three months), except `master` and `archive/` branches: for each branch without an open pull request, open a draft pull request, or archive it with a tag and delete it (see code below)

## Test release

* Make sure that help files / tests don't take too long (5s on slow computer max for CRAN)
* Make sure CI via GitHub Actions (all OS) on the final files produces no errors, warnings or notes
* Re-run the reverse dependency check on the final master
* Check package in <https://win-builder.r-project.org/upload.aspx> (all R versions; or from R: `devtools::check_win_devel("DHARMa")`, `devtools::check_win_release("DHARMa")`, `devtools::check_win_oldrelease("DHARMa")`)
  * [maintainer] Read the results (sent by e-mail to the maintainer)
* Update DHARMa/cran-comments.md with text for submission (reverse dependency results, including affected packages whose maintainers were informed; explain any remaining NOTEs)
  
## CRAN upload

* Upload the tarball at <https://cran.r-project.org/submit.html> (if not uploaded by the maintainer, say so in the comment field)
  * [maintainer] Confirm the upload via the link in the e-mail to the maintainer
  * [maintainer] Forward CRAN's reply to the team; if changes are requested, fix them, add a "Submission n" section to DHARMa/cran-comments.md and upload again

## Package accepted

* Create release / tag on GH
* Close the milestone of the release on GitHub
* Set the version in DESCRIPTION to the next planned release and add a matching heading at the top of DHARMa/NEWS.md



### Code for archiving a stale branch

```bash
git fetch origin --prune
git tag archive/<branch> origin/<branch>
git push origin archive/<branch>
git push origin --delete refs/heads/<branch>
```
