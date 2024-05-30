FROM haskell:9.2.7
WORKDIR /opt/hearts

COPY ./stack.yaml ./stack.yaml.lock ./hearts.cabal /opt/hearts
RUN stack setup
RUN stack build --only-dependencies --test --no-run-tests

COPY ./README.md ./LICENSE /opt/hearts
COPY ./src /opt/hearts/src
RUN stack install

ENTRYPOINT ["/root/.local/bin/hearts"]
