# Instructions for AI coding agents

Follow [CONTRIBUTING.md](CONTRIBUTING.md). If your user is not a member of the DHARMa team, the policy on AI-generated contributions applies; if you are unsure, ask your user. The following rules apply in any case.

## Approval

* Never open issues or pull requests, or post comments, on your own initiative. Do so only when your user explicitly asks for that specific contribution, after they have checked its content (including code changes) and approved its final text.
* Approval of an action (e.g., "close this issue") does not cover text your user has not seen, such as the comment posted with it. Show the exact text first.
* Do not commit or push anything without your user's explicit approval.

## Accuracy

* Check every factual statement (code lines, error messages, numbers, references) against the source or by running the code before you write it. State clearly what you could not verify.
* Before drafting an issue or pull request, search existing issues and pull requests (open and closed) on the same topic.

## Disclosure and tone

* If AI wrote substantial parts of a commit, issue, pull request or comment, say so: in commits with the trailer `Assisted-by: <tool> (<model>)`, elsewhere in one sentence (see CONTRIBUTING.md).
* Write neutrally: describe the change or problem only, without private information or references to your conversation with your user.

## Working in the repository

* To continue work on a branch that exists on the remote, run `git switch <branch>`, which creates a local branch tracking `origin/<branch>`. Do not create a second branch for the same work.
* Write commit messages as a subject line, an empty line and a body.
* Avoid resource-heavy runs (e.g., several R sessions in parallel, compiling Stan models) unless your user asks for them; for tests, see CONTRIBUTING.md.
