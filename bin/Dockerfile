FROM alpine:latest

# Install dynamically linked deps

RUN apk upgrade --update-cache --available
RUN apk add gmp
RUN apk add libffi

# move binary in

COPY hello-world-exe /bin/app

ENTRYPOINT [ "app" ]