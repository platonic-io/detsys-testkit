{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  mvn2nix = import (fetchTarball https://github.com/fzakaria/mvn2nix/archive/master.tar.gz) {};
  mavenRepository =
    mvn2nix.buildMavenRepositoryFromLockFile { file = ./mvn2nix-lock.json; };
  inherit (pkgs) lib stdenv jdk11_headless maven makeWrapper;
  inherit (stdenv) mkDerivation;
in mkDerivation rec {
  pname = "scheduler";
  version = "0.1.0";
  name = "${pname}-${version}";
  src = lib.cleanSource ./.;

  buildInputs = [ jdk11_headless makeWrapper ];
  buildPhase = ''
    export CLASSPATH=$(find ${mavenRepository} -name "*.jar" -printf ':%h/%f')
    export builddir=$TMP/tmp_install
    mkdir -p $builddir
    export res=$builddir/${name}/resources
    mkdir -p $res

    javac java/src/lockfix/LockFix.java -cp $CLASSPATH -d $builddir

    java -cp src:classes:java/src:$CLASSPATH:$builddir \
         -Dclojure.compile.path=$builddir \
         -Dclojure.compiler.direct-linking=true \
         -Dclojure.spec.skip-macros=true \
         clojure.lang.Compile ${pname}.core
  '';

  installPhase = ''
    mkdir -p $out/share/java
    (cd $builddir && jar -cvfe $out/share/java/$name.jar ${pname}.core .)
    mkdir -p $out/bin
    makeWrapper ${jre_headless}/bin/java $out/bin/$name \
        --add-flags "-cp $CLASSPATH:$out/share/java/$name.jar" \
        --add-flags "${pname}.core"
  '';
}
