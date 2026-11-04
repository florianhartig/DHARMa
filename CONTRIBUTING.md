# Contributing to DHARMa

Contributions to DHARMa are welcome! This document describes how to contribute effectively and what to expect from the process.

## Ways to contribute

### Asking for help and reporting issues

DHARMa is open source software, and in the spirit of open source, questions and answers should be public, so that other people can profit from them as well. Please ask questions, report problems and suggest features on the [DHARMa issue tracker](https://github.com/florianhartig/DHARMa/issues). Alternatively, you can post on [Cross Validated](https://stats.stackexchange.com) and e-mail the link to the maintainer, who can then comment there. If you have concerns posting to a public forum, you can also write to the maintainer by e-mail (address in the package DESCRIPTION).

It is easier to help you if your question is well structured. Typically, it contains:

1. the motivation of the study,
2. your model and DHARMa code as you execute it,
3. DHARMa results / error messages,
4. your question.

For problems and errors, please always include a minimal reproducible example: code that runs as is in a fresh R session and shows the problem. See [How to make a great R reproducible example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) and the [reprex](https://cran.r-project.org/package=reprex) package. Often, the problem can be reproduced with simulated data, or with a reduced or randomised version of your data, which is preferred. If you do not want to share your data in public, create the example in public and send the data privately by e-mail. Problem reports without a reproducible example may be closed.

If you have already diagnosed the problem or have ideas for a fix, including that analysis is very helpful. You are also welcome to comment on existing issues, for example by adding examples or proposing solutions.

### Submitting pull requests

Please open or comment on an issue before you start working on a pull request, so that the approach can be discussed before you invest time in an implementation. This avoids situations where a PR cannot be merged because of design conflicts that could have been resolved earlier. Only trivial fixes (e.g., typos or a factual error in the documentation) can go directly to a pull request. See [Pull request workflow](#pull-request-workflow) below.

When working on changes, note that there is extensive code for tests and development purposes outside the core package in the `./Code/` directory on GitHub. You may find useful context there, and development-oriented contributions may also target that section.

If you would like DHARMa to support a further R package or model class, see [Adding support for new R packages](ADDING_PACKAGES.md).

## Policy on AI-generated contributions

**From contributors outside the DHARMa team (maintainers and collaborators with write access to this repository), issues, pull requests and comments created autonomously by AI agents or automated tools will not be accepted.** They will be closed or deleted without review.

The maintainer must be able to understand, evaluate, and take long-term responsibility for every change integrated into DHARMa. AI-generated code frequently appears correct but introduces subtle errors that are difficult to detect without deep familiarity with the codebase, and the burden of reviewing such contributions falls disproportionately on the maintainer. For a scientific package where correctness is critical, this risk is not acceptable.

This policy covers issues, pull requests and comments submitted autonomously by AI agents, as well as contributions where the submitter does not have a full, independent understanding of the proposed changes. Having a tool post a contribution at your explicit request, after you have checked its content, does not count as autonomous.

Using AI tools as a personal aid during development is a matter of individual choice. However, any contribution submitted to this repository must be authored and understood by a human who can explain, defend, and iterate on the proposed changes.

If you are unsure whether your contribution complies with this policy, ask yourself: can you explain every line of your proposed change and its consequences for the rest of the codebase? If not, please do not submit it as a PR — an issue describing the problem or idea is a better starting point.

Instructions for AI coding agents are in [AGENTS.md](AGENTS.md).

## Pull request workflow

`master` is the development branch; CRAN releases are tagged on `master` (e.g. `v0.5.0`). There are no separate development or release branches.

- Create a branch from `master` and name it after the problem and, if there is one, the issue number, e.g. `offsetProblemWithUpdate_351`.
- Keep each pull request focused on one issue and open it against `master`.
- A pull request or commit that resolves an issue should contain `Fixes #351` or `Closes #351` (with your issue number) in its description or commit message, so that GitHub closes the issue automatically when it is merged into `master`.
- The pull request template contains a checklist; please go through it before submitting. Complicated changes are reviewed before they are merged.

## Code and documentation standards

- DHARMa is an R package; the package itself is in the subfolder `DHARMa/`. Please follow the existing code style and conventions:
  - Use meaningful names; function and variable names in lowerCamelCase. Accessor functions start with `get` (e.g. `getSimulations`, `getResiduals`).
  - Help files are written in American English; parameter descriptions start in lower case.
- New functions should include roxygen2 documentation. The help files in `DHARMa/man/` are generated from it; do not edit them by hand, but run `devtools::document("DHARMa")` from the repository root.
- If your contribution changes or adds functionality, please include or update tests in the matching `test*.R` file in `DHARMa/tests/testthat/`. Tests should check behaviour that matters, not cosmetic details.
- Add a short entry to `DHARMa/NEWS.md` under the current version for every user-visible change, ending with the issue number if there is one, e.g. `` * Plot titles of `simulateLRT()` and `testOutliers()` now explain the line colors (#535). ``
- `R CMD check` runs automatically on pull requests to `master` (GitHub Actions) and must pass. Locally, it is usually sufficient to run the tests related to your change, e.g. `testthat::test_file("DHARMa/tests/testthat/testTests.R")` after `devtools::load_all("DHARMa")`.

## Attribution and authorship

Contributors who make substantial contributions to DHARMa will be acknowledged by name in the README and/or the NEWS.md file. Substantial contributions include, but are not limited to: implementing support for a new model class, adding a meaningful new function or test, identifying and fixing a non-trivial bug, or making significant improvements to the documentation or vignette.

Minor corrections such as fixing typos, correcting punctuation, or making small formatting adjustments do not typically result in named acknowledgement, as is standard in open-source scientific software. If you are uncertain whether a planned contribution would qualify, you are welcome to ask in the relevant issue.

## Preparing a CRAN release (maintainers)

See the checklist in [Code/DHARMaCRAN/Checklist.md](Code/DHARMaCRAN/Checklist.md).
