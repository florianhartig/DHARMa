# Maintaining DHARMa

This page is for the DHARMa team (maintainers and collaborators with write access to this repository). The [contributing guidelines](CONTRIBUTING.md) apply to the team as well; this page only adds the tasks that require write access.

## Issues and pull requests

- Set the target release as the milestone of the issue or pull request.
- Team members may merge their own pull requests. If in doubt, request a review from another team member first.
- As an exception to the pull request workflow, trivial changes and changes outside the package folder `DHARMa/` (e.g., `Code/`, `Publications/`; in `DHARMa/` only `NEWS.md` and the version bump after a release) can be committed directly to `master`. CRAN submissions are prepared on a branch, see the [CRAN submission checklist](Code/DHARMaCRAN/Checklist.md).

## Branches

- Existing branches created before the [naming convention](CONTRIBUTING.md#pull-request-workflow) keep their names.
- A branch deleted after a merge can be restored with "Restore branch" on the pull request page.

## Preparing a CRAN release

Follow the [CRAN submission checklist](Code/DHARMaCRAN/Checklist.md). It includes cleaning up stale branches.
