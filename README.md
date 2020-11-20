### detsys-testkit

A test kit for deterministic system tests.

#### How to setup `nix` on your system

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


#### How to setup `nix` for a software component

```bash
cd $component
niv init
niv update nixpkgs -b nixpkgs-unstable
lorri init
cat <<EOF > .envrc
if type lorri &>/dev/null; then
    echo "direnv: using lorri from PATH ($(type -p lorri))"
    eval "$(lorri direnv)"
else
    # fall back to using direnv's builtin nix support
    # to prevent bootstrapping problems.
    use nix
fi
# source an additional user-specific .envrc in ./.envrc-local
if [ -e .envrc-local ]; then
   source .envrc-local
fi
EOF
direnv allow
sed -i -e 's|^{|{ sources ? import ./nix/sources.nix\n,|' \
       -e 's/<nixpkgs>/sources.nixpkgs/' shell.nix
```

Then follow the relevant language support section of the nixpkg
[manual](https://nixos.org/manual/nixpkgs/unstable/#chap-language-support).
