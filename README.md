### Spacemacs documentation tools
*Available at DockerHub as `jare/spacedoc:latest`*

**usage:** `run ACTION [ARGS]...`

**Actions:**
1. `validate  INPUTS...`             Validate input spacedoc(.SDN) files.
2. `describe  SPEC`                  Describe spec by keyword. Example: ":spacedoc.data/root".
3. `relations INPUTS...`             Print node relations in the input Spacedoc(.SDN) files.
4. `format    INPUTS...`             Format .ORG files Specified directly or by parent directory.
5. `export    DOCS_ROOT [INPUTS]...` Export .ORG files to "spacedoc/emacs-tools/export/target/"

*``DOCS_ROOT`` is a root directory of Spacemacs documentation (usually "~/.emacs.d/").*
*If one of`INPUTS` arguments is a directory it will be scanned for .ORG files.*
*`export` called without `INPUTS` will use default list of Spacemacs documentation files.*

**Usage example:**
``` sh
docker run --rm \
  -v <docs_dir>:/tmp/sd/ \
  jare/spacedoc \
  format /tmp/sd/

docker run --rm \
  -v <docs_dir>:/tmp/sd/ \
  -v <out_dir>:/tmp/export \
  jare/spacedoc \
  export /tmp/sd/

docker run --rm \
  -v <out_dir>:/tmp/sd/ \
  jare/spacedoc \
  relations /tmp/sd/

docker run --rm \
  -v <out_dir>:/tmp/sd/ \
  jare/spacedoc \
  validate /tmp/sd/layers/ /tmp/sd/doc/FAQ.sdn
```
