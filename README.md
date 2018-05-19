### Spacemacs documentation tools

#### usage: `run ACTION [OPTIONS]... [ARGS]...`

####  Actions:
 1. `validate  [-i INPUT]+`                 Validate input spacedoc(.SDN) files
 2. `describe  SPEC`                        Describe spec by keyword. Example: ":spacedoc.data/root"
 3. `rel-graph [-i INPUT]+ OUT_FILE`        Draw SVG of node relations in the input spacedoc(.SDN) files
 4. `relations [-i INPUT]+`                 Print node relations in the input Spacedoc(.SDN) files
 5. `format    [FILES/DIRS]...`             Format .ORG files Specified directly or by parent directory
 6. `export    DOCS_ROOT   [FILES/DIRS]...` Export .ORG files to SDN format. Export directory is "spacedoc/emacs-tools/export/target/". `DOCS_ROOT` is a root directory of Spacemacs documentation (usually "~/.emacs.d/"). If an argument is a directory it will be scaned for .ORG files and they will be exported. If only the first argument is supplied a default list of Spacemacs documentation files will be used

**Available at DockerHub as `jare/spacedoc:latest`**

Usage example:
``` sh
docker run --rm -v <docs_dir>:/tmp/docs/ jare/spacedoc/ format /tmp/docs/
docker run --rm -v <docs_dir>:/tmp/docs/ -v <output_dir>:/tmp/export jare/spacedoc export /tmp/docs/
docker run --rm -v <output_dir>:/tmp/docs/ jare/spacedoc validate /tmp/docs/layers
```
