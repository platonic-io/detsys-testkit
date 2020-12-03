{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

pkgs.mkShell {
  # https://fzakaria.com/2020/09/04/mvn2nix-welcoming-maven-into-nix-s-warm-embrace.html
  # https://github.com/fzakaria/mvn2nix
  # clj -Spom
  # nix run -f https://github.com/fzakaria/mvn2nix/archive/master.tar.gz \
  #   --command mvn2nix \
  #   --repositories=https://repo1.maven.org/maven2/ https://repo.clojars.org > \
  #     mvn2nix-lock.json

  buildInputs = [];
}
