FROM oracle/graalvm-ce as graalvm

RUN gu install native-image

WORKDIR /tmp

COPY ./systems/spacedoc-cli/target/spacedoc.jar \
     ./systems/spacedoc-cli/target/observatory.jar ./

RUN native-image \
  --no-server \
  --no-fallback \
  --initialize-at-build-time \
  -H:+ReportUnsupportedElementsAtRuntime \
  -jar /tmp/spacedoc.jar

RUN native-image \
  --no-server \
  --no-fallback \
  --initialize-at-build-time \
  -H:+ReportUnsupportedElementsAtRuntime \
  -jar /tmp/observatory.jar


FROM jare/emacs

COPY --from=graalvm /tmp/spacedoc /tmp/observatory /usr/local/bin

COPY ./scripts/docker/run /opt/spacetools/
COPY ./scripts/docker/spacedoc-cfg.edn /opt/spacetools/
RUN git clone https://github.com/JAremko/sdnize.el.git \
  /opt/spacetools/spacedoc/sdnize

WORKDIR  /opt/spacetools

RUN emacs --batch \
  --eval '(byte-compile-file "spacedoc/sdnize/sdnize.el")' \
  --eval '(byte-compile-file "spacedoc/sdnize/sdnize_worker.el")'

RUN chmod 777 /opt/spacetools/spacedoc/sdnize \
  && chmod 775 /usr/local/bin/spacedoc \
  /usr/local/bin/observatory \
  ./spacedoc/sdnize/sdnize.el \
  ./spacedoc/sdnize/sdnize.elc \
  ./run

RUN apt-get update && apt-get install -y python gnutls-bin gnupg \
  && cd /opt/spacetools/spacedoc/sdnize \
  && git clone https://github.com/cask/cask.git /tmp/caks \
  && /tmp/caks/bin/cask install \
  && /tmp/caks/bin/cask exec buttercup -L . -L tests \
  && apt-get purge -y python \
  && rm -rf .cask /tmp/* /var/lib/apt/lists/*

ENTRYPOINT ["/opt/spacetools/run"]

CMD ["--help"]
