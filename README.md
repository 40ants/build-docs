<a id='x-28DOCS-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# GitHub Action to Build Documentation for a Common Lisp Library

## Table of Contents

- [1 What this action does for you?][a4be]
- [2 A typical usage][3b34]
- [3 Roadmap][0187]

###### \[in package DOCS with nicknames DOCS/DOCS\]
This is a Github Action can be used to build docs and update `gh-pages` branch used to
host static site using [GitHub Pages](https://pages.github.com/).

It should be used after the [setup-lisp](https://40ants.com/setup-lisp/) action.

<a id='x-28DOCS-3A-40FEATURES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 1 What this action does for you?

- It runs [docs-builder](https://40ants.com/docs-builder/) to build `HTML` docs.

- If any there are any changes, it uploads them into the `gh-pages` branch.

- Also, it pushes any other changes to the current branch. This way, README.md
  is updated when you are using [MGL-PAX](https://github.com/cl-doc-systems/mgl-pax)
  or [40ANTS-DOC](https://github.com/40ants/doc)
  as a docs-builder.


<a id='x-28DOCS-3A-40TYPICAL-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 A typical usage

Here is how a minimal GitHub Workflow might look like.

Create a `.github/workflows/docs.yml` file with following content:

```yaml
name: 'Docs'

on:
  # This will run tests on pushes
  # to master branch and every monday:
  push:
    branches:
      - 'main'
      - 'master'
  schedule:
    - cron:  '0 10 * * 1'

jobs:
  build-docs:
    runs-on: ubuntu-latest
    
    env:
      LISP: sbcl-bin

    steps:
      - uses: actions/checkout@v1
      - uses: 40ants/setup-lisp@v1
      - uses: 40ants/build-docs@v1
        with:
          asdf-system: my-system
```

The part, corresponding to an action call is:

```yaml
- uses: 40ants/build-docs@v1
  with:
    asdf-system: my-system
```

Here we provided a system name `my-system`, and
action will figure out how to build it docs, because
it uses smart guesser from
[docs-builder](https://40ants.com/docs-builder/).

You should check if the documentation builder is supported
by `docs-builder` and if not, you can easily to add this support.

<a id='x-28DOCS-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 3 Roadmap

- Add more documentation builders to `docs-builder`.

- Vendor all dependencies, to make action more reliable and secure.


  [0187]: #x-28DOCS-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Roadmap"
  [3b34]: #x-28DOCS-3A-40TYPICAL-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "A typical usage"
  [a4be]: #x-28DOCS-3A-40FEATURES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "What this action does for you?"

* * *
###### \[generated by [40ANTS-DOC](https://40ants.com/doc)\]
