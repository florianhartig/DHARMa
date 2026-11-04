# Contributing to DHARMa

Contributions to DHARMa are welcome! This document describes how to contribute effectively and what to expect from the process. Tasks of the DHARMa team (maintainers and collaborators with write access to this repository), such as preparing a CRAN release, are described in [Maintaining DHARMa](MAINTAINING.md).

## Ways to contribute

### Asking for help and reporting issues

DHARMa is open-source software, and in the spirit of open source, questions and answers should be public, so that other people can profit from them as well. Please ask questions, report problems and suggest features on the [DHARMa issue tracker](https://github.com/florianhartig/DHARMa/issues). Alternatively, you can post on [Cross Validated](https://stats.stackexchange.com) and e-mail the link to the maintainer, who can then comment there. If you have concerns posting to a public forum, you can also write to the maintainer by e-mail (run `maintainer("DHARMa")` in R to get the address).

It is easier to help you if your question is well structured. Typically, it contains:

1. the motivation of the study,
2. your model and DHARMa code as you execute it,
3. DHARMa results / error messages, and the output of `sessionInfo()`,
4. your question.

For problems and errors, please always include a minimal reproducible example: code that runs as is in a fresh R session and shows the problem. See [How to make a great R reproducible example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) and the [reprex](https://cran.r-project.org/package=reprex) package. Use data that everyone can access, if possible: a dataset that comes with R or a package (e.g., `data(mtcars)`), simulated data (e.g., with `DHARMa::createData()`), or a small or randomized part of your data pasted into the example with `dput(head(myData, 30))`. If you do not want to share your data in public, create the example in public and send the data privately by e-mail. Problem reports without a reproducible example may be closed.

If you have already diagnosed the problem or have ideas for a fix, including that analysis is very helpful. You are also welcome to comment on existing issues, for example by adding examples or proposing solutions.

### Submitting pull requests

Please open or comment on an issue before you start working on a pull request, so that the approach can be discussed before you invest time in an implementation. This avoids situations where a pull request cannot be merged because of design conflicts that could have been resolved earlier. Only trivial fixes (e.g., typos or a factual error in the documentation) can go directly to a pull request. See [Pull request workflow](#pull-request-workflow) below.

When working on changes, note that there is extensive code for tests and development purposes outside the package in the folder `Code/` of this repository. You may find useful context there, and development-oriented contributions may also target that section.

If you would like DHARMa to support a further R package or model class, see [Adding support for new R packages](ADDING_PACKAGES.md).

## Policy on AI-generated contributions

This policy applies to contributors outside the DHARMa team.

**Issues, pull requests and comments created autonomously by AI agents or automated tools will not be accepted.** They will be closed or deleted without review.

The maintainer must be able to understand, evaluate, and take long-term responsibility for every change integrated into DHARMa. AI-generated code frequently appears correct but introduces subtle errors that are difficult to detect without deep familiarity with the codebase, and the burden of reviewing such contributions falls disproportionately on the maintainer. For a scientific package where correctness is critical, this risk is not acceptable.

This policy covers issues, pull requests and comments submitted autonomously by AI agents, as well as contributions where the submitter does not have a full, independent understanding of the proposed changes. Having a tool post a contribution at your explicit request, after you have checked its content, does not count as autonomous.

Using AI tools as a personal aid during development is a matter of individual choice. However, any contribution must be authored and understood by a human who can explain, defend, and iterate on the proposed changes. If AI tools wrote substantial parts of your code or text, please mention this briefly in your pull request (one sentence is enough, e.g., "Code drafted with an AI assistant, checked and tested by me.").

If you are unsure whether your contribution complies with this policy, ask yourself: can you explain every line of your proposed change and its consequences for the rest of the codebase? If not, please do not submit it as a pull request — an issue describing the problem or idea is a better starting point.

Instructions for AI coding agents, which apply to everyone, are in [AGENTS.md](AGENTS.md).

## Pull request workflow

`master` is the development branch; CRAN releases are tagged on `master` (e.g., `v0.5.0`). There are no separate development or release branches.

- Members of the DHARMa team create branches in this repository; other contributors work in a fork, where the branch name is up to them.
- Every branch in this repository addresses at least one issue (except trivial fixes, see [Submitting pull requests](#submitting-pull-requests)). Create it from `master` and name it as follows, with `-` as separator and `<shortName>` in lowerCamelCase:
  - one issue: `i<issue number>-<shortName>`, e.g., `i562-jitterPoints`.
  - several closely related issues for the same release: `v<release>-<shortName>`, e.g., `v0.5.1-docFixes`.
  - trivial fixes without an issue: `<shortName>`, e.g., `fixTypoVignette`.
  - Do not name a branch exactly like a release tag (e.g., `v0.5.1`).
- Keep each pull request focused on one issue, or on several closely related issues for the same release, and open it against `master`.
- Once the approach is agreed in the issue, open the pull request early, as a draft if the work is not finished yet, so that work in progress is visible and linked to its issues. If the work is abandoned, close the pull request with a short explanation.
- A pull request or commit that resolves an issue should contain `Fixes #351` or `Closes #351` (with your issue number) in its description or commit message, so that GitHub closes the issue automatically when it is merged into `master`. For several issues, repeat the keyword for each, e.g., `Fixes #567, fixes #568`.
- The pull request template contains a checklist; please go through it before submitting. Pull requests from contributors outside the DHARMa team are reviewed before they are merged.
- After the merge, branches in this repository are deleted automatically.

## Code and documentation standards

- DHARMa is an R package; the package itself is in the subfolder `DHARMa/`. Please follow the existing code style and conventions:
  - Use meaningful names; function and variable names in lowerCamelCase. Accessor functions start with `get` (e.g., `getSimulations`, `getResiduals`).
  - Help files are written in American English; parameter descriptions start in lower case.
- New or changed functions should include roxygen2 documentation. The help files in `DHARMa/man/` are generated from it; do not edit them by hand, but run `devtools::document("DHARMa")` from the repository root.
- If your contribution changes or adds functionality, please include or update tests in the matching `test*.R` file in `DHARMa/tests/testthat/`. Tests should check behavior that matters, not cosmetic details.
- Add a short entry to `DHARMa/NEWS.md` under the topmost version heading for every user-visible change, ending with the issue or pull request number(s) if there are any, e.g., `` * Plot titles of `simulateLRT()` and `testOutliers()` now explain the line colors (#535). ``. For substantial contributions from outside the DHARMa team (see [Attribution and authorship](#attribution-and-authorship)), add the author's GitHub username first, e.g., `(@BertvanderVeen, #535)`.
- `R CMD check` runs on pull requests to `master` (GitHub Actions; for first-time contributors after approval by the DHARMa team) and must pass. Locally, it is usually sufficient to run the tests related to your change, e.g., `testthat::test_file("DHARMa/tests/testthat/testTests.R")` after `devtools::load_all("DHARMa")`.

## Attribution and authorship

Contributors who make substantial contributions to DHARMa will be acknowledged by name in the README and/or the NEWS.md file. Substantial contributions include, but are not limited to: implementing support for a new model class, adding a meaningful new function or test, identifying and fixing a non-trivial bug, or making significant improvements to the documentation or vignette.

Minor corrections such as fixing typos, correcting punctuation, or making small formatting adjustments do not typically result in named acknowledgement, as is standard in open-source scientific software. If you are uncertain whether a planned contribution would qualify, you are welcome to ask in the relevant issue.
