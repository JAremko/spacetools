FROM clojure:openjdk-11-lein as clojure

ENV GENTEST_MULTIPLIER 1

COPY ./ /usr/src/app/

RUN cd /usr/src/app \
  && lein deps \
  && lein polylith build


FROM ubuntu as graalvm

ENV GRAALVM_V=1.0.0-rc13

WORKDIR /tmp

COPY --from=clojure /usr/src/app/systems/spacedoc-cli/target/spacedoc.jar ./

RUN apt-get update && apt-get install -y wget gcc libz-dev

RUN wget --quiet https://github.com/oracle/graal/releases/download/vm-${GRAALVM_V}/graalvm-ce-${GRAALVM_V}-linux-amd64.tar.gz \
    && tar -xzf graalvm-ce-${GRAALVM_V}-linux-amd64.tar.gz

RUN graalvm-ce-${GRAALVM_V}/bin/native-image \
    --no-server \
    -H:+ReportUnsupportedElementsAtRuntime \
    -H:InitialCollectionPolicy='com.oracle.svm.core.genscavenge.CollectionPolicy$NeverCollect' \
    -jar /tmp/spacedoc.jar


FROM jare/emacs

COPY --from=graalvm /tmp/spacedoc /usr/local/bin

COPY ./scripts/docker/run /opt/spacetools/run
COPY ./scripts/sdnize /opt/spacetools/spacedoc/sdnize

WORKDIR  /opt/spacetools

RUN emacs --batch --eval '(byte-compile-file "spacedoc/sdnize/sdnize.el")'
RUN emacs --batch --eval '(byte-compile-file "spacedoc/sdnize/sdnize_worker.el")'

RUN chmod 777 /opt/spacetools/spacedoc/sdnize \
    && chmod 775 /usr/local/bin/spacedoc \
                 ./spacedoc/sdnize/sdnize.el \
                 ./spacedoc/sdnize/sdnize.elc \
                 ./run

RUN apt-get update && apt-get install -y python \
    && cd /opt/spacetools/spacedoc/sdnize \
    && git clone https://github.com/cask/cask.git /tmp/caks \
    && /tmp/caks/bin/cask install \
    && /tmp/caks/bin/cask exec buttercup -L . -L tests \
    && apt-get purge python -y \
    && rm -rf .cask /tmp/* /var/lib/apt/lists/*

ENTRYPOINT ["/opt/spacetools/run"]

CMD ["--help"]
