[![Build Status](https://travis-ci.org/JAremko/spacetools.svg?branch=master)](https://travis-ci.org/JAremko/spacetools)
[![codecov.io](https://codecov.io/github/JAremko/spacetools/coverage.svg?branch=master)](https://codecov.io/github/JAremko/spacetools?branch=master)

### Spacemacs tools
*Available at DockerHub as `jare/spacetools:latest`, `jare/spacetools:noemacs`*

```sh
Spacemacs tools

usage: run ACTION [ARGS]...

Actions:
  validoc ROOT [INPUTS]... Validate Spacemacs documentation files.
                           If only the first argument is supplied
                           the default list of documentation files
                           will be used.
  docfmt  ROOT [INPUTS]... Format Spacemacs documentation files.
                           If only the first argument is supplied
                           the default list of documentation files
                           will be used.

Common arguments:
  ROOT   root directory of Spacemacs documentation. Example: "~/.emacs.d/".
  INPUTS are processed .org files or directories containing them.
```

**Usage example:**

Format all files (add `--user <UID>:<GID>` to keep ownership):

`docker run --rm -v <docs_dir>:/tmp/docs/ jare/spacetools docfmt /tmp/docs/`

Validate:

`docker run --rm -v <docs_dir>:/tmp/docs/ jare/spacetools validoc /tmp/docs/`
