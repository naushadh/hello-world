FROM alpine:latest

# Install dynamically linked deps

RUN apk --no-cache add \
  ca-certificates \
  gmp \
  libffi \
  libpq

# move binary in

COPY hello-postgresql-exe /bin/app

ENTRYPOINT [ "app" ]