# ChangeLog

## 0.2.0

- Switch to multi-stage docker since it dramatically simplifies workflow and makes it easy to visualize builder vs. runtime worlds in one file.
- Upgrade to latest possible Alpine and Stackage resolver. We're bound by the GHC version supported by Alpine -- which in turn sets the resolver and compiler version.

## 0.1.1

- Compacted images by 10-20% with layer reduction and cache purging.

## 0.1.0

- Added `hello-world`: a pure haskell application.
- Added `hello-postgresql`: a haskell app that depends/uses a C FFI lib (postgresql).
- [#1](https://github.com/naushadh/hello-world/pull/1) by [k0ral](https://github.com/k0ral): Fixed a typo.