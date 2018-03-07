# hello-world

A simple hello world app.

A bare minimum haskell application, with a library and executable component to test various development and deployment tools aimed at improving Developer Experience and Operational efficiency.

See specific sections below to learn more.

## Example 1: Ultra light docker image

### Haskell <3 Docker

Unlike most mainstream languages, Haskell applications require very few dependencies from the target machine/host thanks to the RTS (RunTime System) being bundled with the built binary/executable.

One could argue docker is moot; i.e., the app itself is self-contained and uncaring of the host environment dependencies or the lack-thereof. However, we may still want containerization to achieve disposable computation environments. Ex: on a giant beefy central machine, launch an instance of a task or web-service and then tear it down when the need or traffic load subsides.

And docker-izing a Haskell application is rather beautiful, as the `ENTRY POINT` _is_ the executable.

### Why Alpine

Not all container images are equal. You loose the feeling of "light"/cheap instances when images push towards hundreds of MBs or a few GBs (interpreted or VM languages).

Haskell apps require very little from the host OS. So why not cut the chase and get as close to "metal" as possible with a barebones OS? We achieve some nice benefits:

- quick upload (of the whole build app image)
- quick cloud deployment
- reduced attack surface / principle of least privilege; i.e., there isn't really much else besides your app to compromise security

Learn more about [Alpine](https://alpinelinux.org/about/).

### Usage

Haskell applications are best compiled/built on the same architecture/OS as the deployment target. May feel like a hassle when you already have say `stack`/`cabal` already pre-configured, but it's worth it. The cost to set up a "dev" or "build" env from scratch dramatically goes down in the Docker way as you'd only require `Docker` and `stack`. In cases where your app requires say a C lib, having this setup saves trouble from having to communicate OS specific env setup instructions.

#### One time images setup

We setup some images for use with stack later on. Once there are built, we won't have to rebuild them unless their Dockerfile or base image has to be updated.

- Build the builder: an image that will build our apps
  ```bash
  $ pushd docker/builder
  $ docker build --tag builder:latest .
  $ popd
  an image called `builder:latest` should be created
  ```

- Build the runner: an image that run pure haskell apps
  ```bash
  $ pushd docker/runner
  $ docker build --tag runner:latest .
  $ popd
  an image called `runner:latest` should be created
  ```

- Build the runner-libpq: an image that can run a haskell app that depends on libpq
  ```bash
  $ pushd docker/runner-libpq
  $ docker build --tag runner-libpq:latest .
  $ popd
  an image called `runner-libpq:latest` should be created
  ```

#### stack time

The remainder of the flow is entirely `stack` based. For the most part you don't have to think about docker.

- Build apps
  ```bash
  $ stack build --pedantic
  should build app(s) from within an ephemeral container based on the `builder:latest` image
  ```

- Build app images
  ```bash
  $ stack image container
  should build images for every app
  ```

- Run `hello-world` within container
  ```bash
  $ docker run hello-world-exe:latest
  someFunc
  ```

- Run `hello-postgresql` within container
  ```bash
  $ pushd hello-postgresql
  $ docker-compose up --build
  Successfully built e6e8f4785d37
  Successfully tagged bin_exe:latest
  Starting bin_db_1 ... done
  Starting bin_exe_1 ... done
  Attaching to bin_db_1, bin_exe_1
  db_1   | 2018-02-23 00:31:04.742 UTC [1] LOG:  listening on IPv4 address "0.0.0.0", port 5432
  db_1   | 2018-02-23 00:31:04.742 UTC [1] LOG:  listening on IPv6 address "::", port 5432
  db_1   | 2018-02-23 00:31:04.746 UTC [1] LOG:  listening on Unix socket "/var/run/postgresql/.s.PGSQL.5432"
  db_1   | 2018-02-23 00:31:04.761 UTC [17] LOG:  database system was shut down at 2018-02-23 00:27:02 UTC
  db_1   | 2018-02-23 00:31:04.764 UTC [1] LOG:  database system is ready to accept connections
  exe_1  | dbFour
  exe_1  | 4
  bin_exe_1 exited with code 0
  ```

- Marvel at how compact app images are
  ```bash
  $ docker images
  REPOSITORY             TAG                 IMAGE ID            CREATED              SIZE
  hello-postgresql-exe    latest              520c289ff2d5        15 seconds ago      20.6MB
  hello-world-exe         latest              46d08020b40c        15 seconds ago      6.05MB
  ```

- Marvel at how (relatively) compact even our builder is
  ```bash
  $ docker images
  REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
  builder                 latest              cd6b16dd1f50        4 minutes ago       1.04GB
  haskell                 latest              4bbdbe2913ed        2 weeks ago         1.05GB
  fpco/stack-build        latest              143c6892a663        2 weeks ago         7.26GB
  ```

### Caveats

- GHC version
  - `stack` in docker mode will use system GHC by default
  - the version of `ghc` shipped with a stackage resolver MUST match the version of `ghc` installed (via `APK` or stack bin-dist injection).
  - otherwise `stack` will try to build `ghc` from source. This usually doesn't go so well.
  - the official alpine ghc version can be found [here](https://pkgs.alpinelinux.org/packages?name=ghc&branch=edge).

### References/Inspirations

- [jkachmar's GIST](https://gist.github.com/jkachmar/4828bfe0f585bec93878ea893c3373ee)
- [Mark Bucciarelli's blog post](http://markbucciarelli.com/posts/2017-04-05_haskell_on_alpine_linux.html)
