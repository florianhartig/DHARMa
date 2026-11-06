# Submission Checklist

Steps marked [maintainer] need e-mails sent to the maintainer; all other steps can be done by any team member.

## Prepare release

* Run the GitHub Actions workflow "Reverse dependency check" early (Actions tab, "Run workflow")
  * If an intended change breaks packages: inform their maintainers with suggested changes at least 2 weeks before submitting
* Check current CRAN issues at <https://cran.r-project.org/web/checks/check_results_DHARMa.html>
* All GitHub issues and pull requests in the release milestone resolved, or postponed to the next release?
  * All new features / changes have help files, examples and appropriate unit tests? 
  * Is the NEWS.md file updated?
  * Are all documentation files, including README.md, vignettes, updated?
* Update DESCRIPTION files -> date, version number, etc.
* Make sure a local `R CMD check --as-cran` on the tarball from `R CMD build DHARMa` (needs pandoc for the vignettes) produces no errors, warnings or notes
  * `devtools::document("DHARMa")` produces no new changes (check with `git status`)
* Go through stale branches (GitHub branches page, "Stale" view = no commits in the last three months), except `master` and `archive/` branches: for each branch without an open pull request, open a draft pull request, or archive it with a tag and delete it (see code below)

## Test release

* Make sure that help files / tests don't take too long (5s on slow computer max for CRAN)
* Make sure CI via GitHub Actions (all OS) on the final master produces no errors, warnings or notes
* Re-run the reverse dependency check on the final master
* Check package in <https://win-builder.r-project.org/upload.aspx> (all R versions; or from R: `devtools::check_win_devel("DHARMa")`, `devtools::check_win_release("DHARMa")`, `devtools::check_win_oldrelease("DHARMa")`)
  * [maintainer] Read the results
* Update DHARMa/cran-comments.md with text for submission (reverse dependency results, including affected packages whose maintainers were informed; explain any remaining notes)
  
## CRAN upload

* Build the tarball from the final master and upload it at <https://cran.r-project.org/submit.html>, with the new section of DHARMa/cran-comments.md in "Optional comment" (if not uploaded by the maintainer, explain this there)
  * [maintainer] Confirm the upload via the link in CRAN's e-mail
  * [maintainer] Forward CRAN's reply to the team
  * If changes are requested: fix them, add a "Submission n" section to DHARMa/cran-comments.md that says how they were addressed, and upload again as above

## Package accepted

* On GitHub: create a release with tag `v<version>` on `master` and close the release milestone
* Set the version in DESCRIPTION to the next planned release and add a matching heading at the top of DHARMa/NEWS.md



### Code for archiving a stale branch

```bash
git fetch origin --prune
git tag archive/<branch> origin/<branch>
git push origin archive/<branch>
git push origin --delete refs/heads/<branch>
```
