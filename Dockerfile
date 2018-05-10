FROM clojure:latest as clojure

COPY clj-tools/spacedoc /usr/src/app

RUN cd /usr/src/app && lein uberjar


FROM ubuntu:latest as graalvm

ENV GRAALVM_V=1.0.0-rc1

WORKDIR /tmp

COPY --from=clojure /usr/src/app/target/uberjar/spacedoc.jar ./

RUN apt-get update && apt-get install -y wget gcc libz-dev

RUN wget --quiet https://github.com/oracle/graal/releases/download/vm-${GRAALVM_V}/graalvm-ce-${GRAALVM_V}-linux-amd64.tar.gz \
    && tar -xvzf graalvm-ce-${GRAALVM_V}-linux-amd64.tar.gz

RUN graalvm-${GRAALVM_V}/bin/native-image -H:+ReportUnsupportedElementsAtRuntime -jar /tmp/spacedoc.jar


FROM ubuntu:latest

COPY --from=graalvm /tmp/spacedoc /usr/local/bin
