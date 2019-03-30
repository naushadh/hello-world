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

Haskell applications are best compiled/built on the same architecture/OS as the deployment target. May feel like a hassle when you already have say `stack`/`cabal` already pre-configured, but it's worth it. The cost to set up a "dev" or "build" env from scratch dramatically goes down in the Docker way as you'd only require `Docker`. In cases where your app requires system/foreign dependencies (ex: a C lib), the Docker way ensures you work with the exact same runtime from development to production.

- Build and run `hello-world` (a pure haskell CLI app)

  ```bash
  $ docker-compose build hello-world # builds, tests, packages executable(s), and produces a tagged Docker image "hello-world:latest"
  $ docker-compose run hello-world
  someFunc
  ```

- Build and run `hello-postgresql` (a haskell CLI app with C lib dependency)

  ```bash
  $ docker-compose build hello-postgresql # builds, tests, packages executable(s), and produces a tagged Docker image "hello-postgresql:latest"
  $ docker-compose up --build
  Creating hello-world_hello-world_1 ... done
  Creating hello-world_postgres_1    ... done
  Creating hello-world_hello-postgresql_1 ... done
  hello-postgresql_1  | dbFour
  hello-postgresql_1  | 4
  hello-world_hello-postgresql_1 exited with code 0
  ```

- Marvel at how compact app images are

  ```bash
  $ docker images
  REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
  hello-postgresql    latest              04198bd1418d        8 minutes ago       21.6MB
  hello-world         latest              6f085d084f4a        About an hour ago   7.36MB
  ```

### Caveats

- GHC version
  - the version of `ghc` shipped with a stackage resolver MUST match the version of `ghc` available on Alpine.
  - otherwise `stack` will try to build `ghc` from source. This usually doesn't go so well.
  - the official alpine ghc version can be found [here](https://pkgs.alpinelinux.org/packages?name=ghc&branch=edge).

### References/Inspirations

- [jkachmar's GIST](https://gist.github.com/jkachmar/4828bfe0f585bec93878ea893c3373ee)
- [Mark Bucciarelli's blog post](http://markbucciarelli.com/posts/2017-04-05_haskell_on_alpine_linux.html)
