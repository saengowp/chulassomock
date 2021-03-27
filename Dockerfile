FROM haskell:8.8.4 AS build

RUN mkdir -p /opt/chulassomock
WORKDIR /opt/chulassomock

COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --dependencies-only

COPY . .
RUN stack build
RUN cp $(stack path --local-install-root)/bin/chulassomock chulassomock
RUN tar cfv share.tar /root/.stack/snapshots/*/*/*/share/

FROM ubuntu:18.04
COPY --from=build /opt/chulassomock/chulassomock /
COPY --from=build /opt/chulassomock/share.tar /
RUN tar xvf share.tar

ENTRYPOINT ["/chulassomock"]
