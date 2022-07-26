# NUKLEUS-ECU Contributing Guide

:tada: Thanks for taking the time to contribute! :tada:

The following is a set of guidelines for contributing to NUKLEUS-ECU on GitHub. 
These are mainly guidelines, not rules. Use your own judgment and suggest changes to this document in a pull request.

#### Table Of Contents

[I just have a question!](#i-just-have-a-question)

[How Can I Contribute?](#how-can-i-contribute)
  * [Reporting Bugs](#reporting-bugs)
  * [Suggesting Enhancements](#suggesting-enhancements)
  * [Your First Code Contribution](#your-first-code-contribution)
  * [Pull Requests](#pull-requests)

[Styleguides](#styleguides)

[Additional Notes](#additional-notes)
  * [Issue and Pull Request Labels](#issue-and-pull-request-labels)


## I just have a question!

If you have any questions, it is best to send an email to ecu@napkon.de.

## How Can I Contribute?

### Reporting Bugs

This section guides you through submitting a bug report for Atom. Following these guidelines helps maintainers and the community understand your report :pencil:, reproduce the behavior :computer: :computer:, and find related reports :mag_right:.

Before creating bug reports, please check [this list](#before-submitting-a-bug-report) as you might find out that you don't need to create one. When you are creating a bug report, please [include as many details as possible](#how-do-i-submit-a-good-bug-report). Fill out [the required issue template](ISSUE_TEMPLATE.md), the information it asks for helps us resolve issues faster.

> **Note:** If you find a **Closed** issue that seems like it is the same thing that you're experiencing, open a new issue and include a link to the original issue in the body of your new one.

#### Before Submitting A Bug Report

* **Perform a [cursory search]([https://github.com/search?q=+is%3Aissue+user%3Aatom](https://github.com/search?q=+is%3Aissue+user%3Anukleus-ecu)** to see if the problem has already been reported. If it has **and the issue is still open**, add a comment to the existing issue instead of opening a new one.

#### How Do I Submit A Good Bug Report?

Bugs are tracked as [GitHub issues](https://guides.github.com/features/issues/). Create an issue on the respective repository and provide the following information by filling in [the template](ISSUE_TEMPLATE.md).

Explain the problem and include additional details to help maintainers reproduce the problem:

* **Use a unique and descriptive title** for the problem to identify the problem.
* **Describe the exact steps that reproduce** the problem in as much detail as possible. For example, start by explaining how you executed the code, such as exactly what command you used in the terminal or how else you started the code. When listing steps you did, **don't just say what you did, but explain how you did it**. For example, if you performed primary coding, explain exactly what function you used.
* **Provide examples to demonstrate the steps**. Include links to files or GitHub projects or copy-and-paste snippets that you use in these examples. If you provide snippets in the question, use [markdown code blocks] (https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the behavior you observed after following the steps** and point out what exactly the problem is with that behavior.
* **Explain what behavior you expected instead and why**.
* **Include screenshots and/or animated GIFs** that show you performing the steps described and clearly demonstrate the problem.
* **If you report that the code triggers errors**, include a crash report with a stack trace. 

Provide more context by answering these questions:

* **Did the problem start happening recently** (e.g. after updating to a new version) or was this always a problem?
* If the problem started happening recently, **can you reproduce the problem in an older version of the code?** What's the most recent version in which the problem doesn't happen? You can download older versions from respective releases pages of the repository.
* **Can you reliably! reproduce the issue?** If not, provide details about how often the problem happens and under which conditions it normally happens.

Include details about your configuration and environment:

* **Which version of the code are you using?**
* **What's the name and version of the OS you're using**?
* **Are you running the code in a virtual machine?** If so, which VM software are you using and which operating systems and versions are used for the host and the guest?
* **Which [packages](#atom-and-packages) do you have installed?**

### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion, including completely new features and minor improvements to existing functionality. Following these guidelines helps maintainers and the community understand your suggestion :pencil: and find related suggestions :mag_right:.

Before creating enhancement suggestions, please check [this list](#before-submitting-an-enhancement-suggestion) as you might find out that you don't need to create one. When you are creating an enhancement suggestion, please [include as many details as possible](#how-do-i-submit-a-good-enhancement-suggestion). Fill in [the template](ISSUE_TEMPLATE.md), including the steps that you imagine you would take if the feature you're requesting existed.

#### Before Submitting An Enhancement Suggestion

* **Check if there's already a package or code somewhere else in the net which provides that enhancement.**
* **Perform a [cursory search](https://github.com/search?q=+is%3Aissue+user%3Anukleus-ecu)** to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.

#### How Do I Submit A Good Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://guides.github.com/features/issues/). Create an issue on the respective repository and provide the following information:

* **Use a unique and descriptive title** for the problem to identify the problem.
* **Provide a step-by-step description of the suggested enhancement** in as many details as possible.
* **Provide specific examples to demonstrate the steps**. Include links to files or copy/pasteable snippets which you use in those examples, as [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the current behavior** and point out what is your **preferred behavior**.
* **Explain why this enhancement would be useful**
* **Specify the version of the code you're using.**
* **Specify the name and version of the OS you're using.**

### Your First Code Contribution

If you are unsure where to begin contributing you can start by looking through issues with tag `help-wanted`:

### Pull Requests

Please follow these steps to have your contribution considered by the maintainers:

1. Follow all instructions in [the template](PULL_REQUEST_TEMPLATE.md)
2. Follow the [styleguides](#styleguides)
3. After you submit your pull request, verify that all [status checks](https://help.github.com/articles/about-status-checks/) are passing <details><summary>What if the status checks are failing?</summary>If a status check fails and you believe the error is unrelated to your change, please leave a comment on the pull request explaining why you believe the error is unrelated. A maintainer will re-run the status check for you. If we conclude that the error was a false positive, then we will open a message to track the issue with our status check suite.</details>

Although the above requirements must be met before your Pull Request can be reviewed, the reviewer(s) may require you to perform additional design work, testing, or other changes before your Pull Request can be finally accepted.

## Styleguides

### Git Commit Messages

* We use [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/)
* Therefore, please consider using [commitizen](https://github.com/commitizen/cz-cli).
* Use the present tense ("Add feature" not "Added feature")
* Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
* Limit the first line to 72 characters or less
* Reference issues and pull requests after the first line

### R Styleguide

All R code must adhere to [Googles R Standard Style](https://google.github.io/styleguide/Rguide.html).


## Additional Notes

### Issue and Pull Request Labels

This section lists the labels we use to help us track and manage issues and pull requests. 

#### Type of Issue and Issue State

| Label name | Description |
| --- | --- |
| `enhancement` | Feature requests. |
| `bug` | Confirmed bugs or reports that are very likely to be bugs. |
| `question` | Questions more than bug reports or feature requests (e.g. how do I do X). |
| `feedback` | General feedback more than bug reports or feature requests. |
| `help-wanted` | For this type of issue we would appreciate help from the community. |
