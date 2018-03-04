FROM fpco/stack-build:lts-9 as build
WORKDIR root
COPY . root
RUN stack init
RUN stack build
RUN cp $(stack path --local-install-root)/bin/hello-world-exe /bin/hello-world-exe
FROM alpine:latest
RUN apk --no-cache add ca-certificates gmp libffi
WORKDIR /root
COPY --from=build /bin/hello-world-exe /bin/app
CMD ["app"]