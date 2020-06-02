FROM clojure:openjdk-14-lein as clojure

ENV GENTEST_MULTIPLIER 1

COPY ./ /usr/src/app/

RUN cd /usr/src/app \
  && lein deps \
  && lein polylith build


FROM ubuntu as graalvm

ENV GRAALVM_V=20.1.0

WORKDIR /tmp

COPY --from=clojure /usr/src/app/systems/spacedoc-cli/target/spacedoc.jar ./

RUN apt-get update && apt-get install -y wget gcc libz-dev

RUN wget --quiet "https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-${GRAALVM_V}/graalvm-ce-java11-linux-amd64-${GRAALVM_V}.tar.gz" \
  && tar -xzf "graalvm-ce-java11-linux-amd64-${GRAALVM_V}.tar.gz"

RUN graalvm-ce-java11-${GRAALVM_V}/bin/gu install native-image

RUN graalvm-ce-java11-${GRAALVM_V}/bin/native-image \
  --no-server \
  --no-fallback \
  --initialize-at-build-time \
  -H:+ReportUnsupportedElementsAtRuntime \
  -jar /tmp/spacedoc.jar


FROM jare/emacs

COPY --from=graalvm /tmp/spacedoc /usr/local/bin

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
