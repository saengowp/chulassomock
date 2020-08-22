FROM ubuntu:bionic as build

RUN apt-get update
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh

RUN mkdir -p /opt/chulassomock
WORKDIR /opt/chulassomock
RUN stack setup 8.6.5

COPY . /opt/chulassomock
RUN stack build
RUN cp $(stack path --local-install-root)/bin/chulassomock chulassomock

FROM ubuntu:bionic
COPY --from=build /opt/chulassomock/chulassomock .
ENTRYPOINT ["/chulassomock"]
