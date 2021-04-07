### Nix

#### Development workflow

Once you got `nix` installed, it should be a matter of typing
`nix-shell` at the root of the repo to get the development environment
setup as well as access to all binaries, in particular the `detsys`
command-line interface tool.

If you change some detsys component, you can exit the`nix-shell` and
reenter it again, this should only rebuild the change component and any
dependencies of it.

If you find `nix-shell` rebuilding more than necessary, then you can
also try to run `nix-env --install --file default.nix` from within the
directory of the component that gets unnecessarily rebuilt. This is the
`nix` equivalent of `make install`, you can also do `nix-build` from
within the directory of a component to see if the `default.nix` builds
(equivalent to `make`).

If you want to more iteratively develop your component, you can also use
whatever language specific compiler or build tool the component uses.
Note however that `detsys` will use whatever binary it finds first in
your `PATH`, which could be old versions installed by `nix-env`. All
`detsys` components come with a `--version` flag and the `detsys`
command-line interface tool also has a `versions` subcommand to help you
ensure that you are using the intended versions of the components.

This entering `nix-shell`, leaving and reentering it again is obviously
annoying. `lorri` is beefed up version of `nix-shell` that is supposed
to address some of these shortcomings, but we've still not figured out
how to use it.

#### Why `nix` over `docker`?

* Just `cd` into the directory and it automatically makes all your tools
available natively, e.g. `which go` returns
`/nix/store/yqj2hbv541fsi1jflb9ay0yks3c0fi29-go-1.15.4/bin/go`. Yes, you
can have a development `Dockerfile` which contains your compiler etc,
but would you also use for example `git` or `emacs` via `docker`? I
guess you could, but it seems clunky;

* the caching is better, on a per dependency level rather than layer
  level. For example, in docker if you add a new dependency to your
  `apt-get install` list then all dependencies get downloaded again (and
  all subsequent layers rebuilt) or if you add the same dependency to
  two different Dockerfiles then it will be downloaded twice;

* cache can be shared among team members, CI and customers, so that each
  build only ever gets built once (not tried yet, see
  https://cachix.org/);

* it works on MacOS also, as opposed to being an afterthought like in
  docker;

* no mounting of volumes;

* no problem with all files being created as root;

* actually reproducible as opposed to pretending to be (you can set
  versions to :latest in docker, which isn't reproducible, you can
  download arbitrary files using curl which isn't reproducible);

* if you do pin all versions in docker, then keeping things up to date
  is a lot of work (you need to manually go through and bumb versions in
  all your Dockerfiles);

* ever tried to start a docker container from a docker container? With
  mounted volumes?

* from a nix description you can derive a docker image, .deb, or .rpm
  (not tried this yet);

* something something CI (not looked into this yet).

#### How to setup `nix` on your system

The steps below are taken from the following [blog
post](https://christine.website/blog/how-i-start-nix-2020-03-08), please consult
it or the documentation for the individual tools for an understanding of their
purpose.

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

#### How to setup `nix` for a software component

TODO(stevan): the following is slightly outdated, we currently have a
single `niv init` at the top-level which all components use.

```bash
cd $component
niv init
niv update nixpkgs -b nixpkgs-unstable
lorri init
cat <<'EOF' > .envrc
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
sed -i -e 's|^{|{ sources ? import ./../../nix/sources.nix\n,|' \
       -e 's/<nixpkgs>/sources.nixpkgs/' shell.nix
# gitignoreSource?
```

Then follow the relevant language support section of the `nixpkgs`
[manual](https://nixos.org/manual/nixpkgs/unstable/#chap-language-support).

#### Nix resources

* General overview, in [writing](https://shopify.engineering/what-is-nix) or as
  a [presentation](https://www.youtube.com/watch?v=6iVXaqUfHi4);

* Nix [Pills](https://nixos.org/guides/nix-pills/);
* `nixpkgs` [manual](https://nixos.org/manual/nixpkgs/unstable/);
* `nix` [manual](https://nixos.org/manual/nix/unstable/);
* [How to Learn Nix](https://ianthehenry.com/posts/how-to-learn-nix/).
