FROM alpine:3.9 as builder

RUN apk --no-cache add ghc musl-dev zlib-dev postgresql-dev
RUN wget -qO- https://get.haskellstack.org/ | sh

# Pre-install deps so we can re-use cached layers
# https://github.com/freebroccolo/docker-haskell/issues/54#issuecomment-283222910
COPY stack.yaml ./
COPY package.yaml ./
RUN stack setup
RUN stack install --dependencies-only

COPY LICENSE ./
COPY app ./app
COPY src ./src
COPY test ./test

RUN stack build --pedantic --test --copy-bins --local-bin-path /tmp/dist/

FROM alpine:3.9 as runtime

RUN apk --no-cache add ca-certificates gmp libffi libpq
COPY --from=builder /tmp/dist/* /usr/local/bin/
CMD hello-postgresql-exe
