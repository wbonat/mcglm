# Instructions for contributing

This guide contains instructions for collaborators to contribute with
the `mcglm` package.

The general guidelines for contributing to any of the projects in this
GitLab can be found in [Boas práticas do uso do Git e GitLab no LEG][]
(in portuguese only).

## Workflow

In `mcglm` we use the Gitflow workflow, which can be viewed in these
two links: [A successful Git branching model][], and
[Atlassian tutorial][].

Basically, all the ongoing development is made in the `devel`
branch. New features and bug fixes must are made in additional (parallell)
branches such like `issue#<num>`, where `<num>` is an incremental
number. Each new feature or bug fix must have a corresponding issue,
created in the GitLab interface. In this issue you must describe in
details what are the things to do (or fix). The number of the issue is
created automatically by GitLab, and this number must correspond to the
`<num>` in the branch name. After someone (yourself or others) have
finished working in the issue, in branch `issue#<num>`, he must send a
Merge Request (MR), preferably to @wbonat, to merge this branch to
`devel`.

Basically, the workflow follows this steps:

1. All the ongoing work is done in the `devel` branch
2. When it is decided that a new version is ready for release, it is
   moved to the `master` branch, where it receives a tag with the version
   number, and nothingmore is added in the `master` branch

To suggest or add new features or bug reports:

1. Open an issue in the GitLab interface
2. The person who will work on this issue must then create a new
   branch (from `devel`) with the name `issue#<num>`, where `<num>` is
   the number of the issue
3. When the work on this issue is finished, the person must create a
   Merge Request (MR) to incorporate changes from `issue#<num>` to
   `devel`

The `issue#<num>` branches are merged with the `devel` branch, where
final tests are made to guarantee it is stable. Only then it is moved to
the `master` branch. Note that we don't use a `release` branch as in a
traditional Gitflow workflow (this is the only difference).

This way, users may opt to install the stable version, from the `master`
branch, or they may install the development version from the `devel`
branch.

To contribute to the project, please review the commits in the `devel`
branch, and the list of issues (opened and closed) to make sure that
what you are trying to do is not already being done.

Also, if you're not sure about how to do something, but have an idea for
the project, feel free to open an issue and explain what you want from
there.

## General rules for commits

Commits are a very important part of git. The four most important rules
to follow are:

1. **Commit often**
2. **Don't commit half-done work**
3. **Test before you commit**
4. **Write good commit messages**

For more detailed explanation and specific guidelines, see
[Criando commits][] in [Boas práticas do uso do Git e GitLab no LEG][],
and [Version Control Best Practices][].

****

Fell free to contact us (via email or an issue) if you have any doubts
about contributing.

<!-- links -->

[A successful Git branching model]: http://nvie.com/posts/a-successful-git-branching-model/
[Atlassian tutorial]: https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow
[Boas práticas do uso do Git e GitLab no LEG]: http://git.leg.ufpr.br/leg/gitlab-rautu/blob/master/CONTRIBUTING.md
[Version Control Best Practices]: http://www.git-tower.com/learn/git/ebook/command-line/appendix/best-practices
[Criando commits]: http://git.leg.ufpr.br/leg/gitlab-rautu/blob/master/CONTRIBUTING.md#criando-commits
