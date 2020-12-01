{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  mvn2nix = import (fetchTarball https://github.com/fzakaria/mvn2nix/archive/master.tar.gz) {};
  mavenRepository =
    mvn2nix.buildMavenRepositoryFromLockFile { file = ./mvn2nix-lock.json; };
in stdenv.mkDerivation rec {
  pname = "scheduler";
  version = "0.1.0";
  name = "${pname}-${version}";
  src = lib.cleanSource ./.;

  buildInputs = [ clojure jdk11_headless makeWrapper ];
  buildPhase = ''
    export CLASSPATH=$(find ${mavenRepository} -name "*.jar" -printf ':%h/%f')
    export builddir=$TMP/classes
    mkdir -p $builddir

    javac java/src/lockfix/LockFix.java -cp $CLASSPATH -d $builddir

    clj -Scp src:$CLASSPATH:$builddir \
      -J-Dclojure.compile.path=$builddir \
      -M -e "(compile (quote ${pname}.core))"
  '';

  installPhase = ''
    echo "Main-Class: ${pname}.core" > manifest.txt
    echo "Class-Path: ." >> manifest.txt
    find ${mavenRepository} -name '*.jar' -printf '  %h/%f\n' >> manifest.txt

    mkdir -p $out/share/java
    jar cvfm $out/share/java/$name.jar manifest.txt -C $builddir .
    mkdir -p $out/bin
    makeWrapper ${jre_headless}/bin/java $out/bin/${pname} \
      --add-flags "-jar $out/share/java/$name.jar"
  '';
}
