FROM alpine:latest

# Install dynamically linked deps

RUN apk --no-cache add \
  ca-certificates \
  gmp \
  libffi

# move binary in

COPY hello-world-exe /bin/app

ENTRYPOINT [ "app" ]