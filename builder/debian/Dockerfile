FROM bitnami/minideb:stretch

# Instructions inspired from ./../alpine/Dockerfile

## Lifted from: https://github.com/freebroccolo/docker-haskell/blob/edb625b37f26b2eb072f1f3b5fd9f884b04a6c2f/8.0/Dockerfile
## Add hvr-PPA: https://launchpad.net/%7Ehvr/+archive/ubuntu/ghc
## This is to make ghc-* available
RUN   apt-get update \
  &&  apt-get install -y gnupg \
  &&  echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > /etc/apt/sources.list.d/ghc.list \
  &&  apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 \
  &&  apt-get update

RUN install_packages \
  ghc-8.0.2 \
  ## stack deps
  haskell-stack \
  ## app deps
  libpq-dev

RUN ln -s /usr/include/postgresql/libpq /usr/include/

ENV PATH /opt/ghc/8.0.2/bin:$PATH

# ENTRYPOINT [ "/bin/sh" ]
