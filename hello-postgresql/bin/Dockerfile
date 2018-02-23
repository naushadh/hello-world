FROM alpine:latest

# Install dynamically linked deps

RUN apk upgrade --update-cache --available

## Core deps
RUN apk add gmp libffi
## App deps
RUN apk add libpq

# move binary in

COPY hello-postgresql-exe /bin/app

ENTRYPOINT [ "app" ]