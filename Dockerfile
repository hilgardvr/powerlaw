FROM alpine:3.19.1
RUN apk add binutils-gold curl gcc g++ gmp-dev libc-dev libffi-dev libpq-dev make musl-dev ncurses-dev perl tar xz zlib-dev

RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ARG GHC=9.4.8
ARG CABAL=3.10.2.1
RUN ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

WORKDIR /opt/powerlaw
RUN cabal update

COPY ./powerlaw.cabal /opt/powerlaw/powerlaw.cabal
RUN cabal build --only-dependencies

COPY ./app /opt/powerlaw/app/
COPY ./migrations /opt/powerlaw/migrations/
COPY ./.env /opt/powerlaw/
RUN cabal build
CMD ["cabal", "run"]
