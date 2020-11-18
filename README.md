### detsys

Deterministic system tests.

#### Nix setup

1. Install [`nix`](https://nixos.org/download.html#nix-verify-installation) with
   `curl -L https://nixos.org/nix/install | sh`;
2. Install [`lorri`](https://github.com/target/lorri/) with `nix-env --install
   --file https://github.com/target/lorri/archive/master.tar.gz`;
3. Enable starting `lorri` as a service following
   [this](https://github.com/target/lorri/blob/master/contrib/daemon.md#how-to-start-the-lorri-daemon-as-a-service)
   document;
4. Install [`direnv`](https://direnv.net/) with `nix-env --install direnv`;
5. [Configure](https://direnv.net/docs/hook.html) your shell to use `direnv`;
6. Install [`niv`](https://github.com/nmattia/niv) with `nix-env --install niv`.

The above steps are taken from the following [blog
post](https://christine.website/blog/how-i-start-nix-2020-03-08), please consult
it or the documentation for the individual tools for an understanding of their
purpose.
