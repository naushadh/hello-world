FROM alpine:latest

# Instructions lifted from:
# http://markbucciarelli.com/posts/2017-04-05_haskell_on_alpine_linux.html

RUN apk upgrade --update-cache --available
RUN apk add ghc
RUN apk add curl musl-dev zlib-dev

# Required for stack's 'usermod' and 'groupmod' invocations
RUN apk add shadow

# stack deps
RUN apk add xz make

# ENTRYPOINT [ "/bin/sh" ]
