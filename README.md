# [haskell.nix](https://github.com/input-output-hk/haskell.nix) examples

This repository serves as an example of haskell.nix usage. It's kept to the
very basic minimum on purpose. The idea is to ensure that we do not conflate
complex project setups with the core of haskell.nix.

## Objectives

The objective of haskell.nix is to simplify the building of Haskell packages by
minimizing the need for customization. It achieves this through the use of
Import From Derivation (IFD) expressions, which allow for dynamic adaptation to
Haskell projects without modifying the Nix code. While IFDs may have performance
drawbacks and are not allowed in nixpkgs, they provide flexibility in the
context of haskell.nix.

The `flake.nix` has been kept intentionally simple, with as little abstraction
as possible.  While this leads to a more verbose `flake.nix`, our hope is that
it makes it much easer to read and follow along.

## Build Targets 

Running `nix flake show --all-systems --option allow-import-from-derivation true`
will yield:
```
git+file:///.../haskell-nix-example
├───defaultPackage
│   ├───aarch64-darwin: package 'hello-exe-hello-1.0.0.2'
│   ├───aarch64-linux: package 'hello-exe-hello-1.0.0.2'
│   ├───x86_64-darwin: package 'hello-exe-hello-1.0.0.2'
│   └───x86_64-linux: package 'hello-exe-hello-1.0.0.2'
└───packages
    ├───aarch64-darwin
    │   └───hello: package 'hello-exe-hello-1.0.0.2'
    ├───aarch64-linux
    │   └───hello: package 'hello-exe-hello-1.0.0.2'
    ├───x86_64-darwin
    │   └───hello: package 'hello-exe-hello-1.0.0.2'
    └───x86_64-linux
        ├───hello: package 'hello-exe-hello-1.0.0.2'
        ├───hello-dynamic: package 'hello-exe-hello-aarch64-unknown-linux-gnu-1.0.0.2'
        ├───hello-javascript: package 'hello-exe-hello-1.0.0.2'
        ├───hello-mingw: package 'hello-exe-hello-x86_64-w64-mingw32-1.0.0.2'
        ├───hello-static-musl: package 'hello-exe-hello-aarch64-unknown-linux-musl-1.0.0.2'
        └───hello-ucrt64: package 'hello-exe-hello-x86_64-w64-mingw32-1.0.0.2'
```

We can then get packages for each cross target as needed.
