### Spacemacs documentation tools
*Available at DockerHub as `jare/spacedoc:latest`*

```sh
Spacemacs documentation tools

usage: run ACTION [ARGS]...

Actions:
  validate  ROOT [INPUTS]... Validate Spacemacs documentation files.
                             If only the first argument is supplied
                             the default list of documentation files
                             will be used.
  format    ROOT [INPUTS]... Format Spacemacs documentation files.
                             If only the first argument is supplied
                             all files in ROOT folder will be formatted.

Common arguments:
  ROOT   root directory of Spacemacs documentation. Example: "~/.emacs.d/".
  INPUTS are processed .org files or directories containing them.
```

**Usage example:**

Format all files:
`docker run --rm -v <docs_dir>:/tmp/docs/ jare/spacedoc format /tmp/docs/`

Validate:
`docker run --rm -v <docs_dir>:/tmp/docs/ jare/spacedoc validate /tmp/docs/`
