{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  mvn2nix = import (fetchTarball https://github.com/fzakaria/mvn2nix/archive/master.tar.gz) {};
  mavenRepository =
    mvn2nix.buildMavenRepositoryFromLockFile { file = ./mvn2nix-lock.json; };
  graalvm = (callPackage ./graalvm.nix {}).graalvm11-ce;
in stdenv.mkDerivation rec {
  pname = "checker";
  version = "0.1.0";
  name = "${pname}-${version}";
  src = lib.cleanSource ./.;

  buildInputs = [ clojure jdk11_headless graalvm ];

  buildPhase = ''
    export CLASSPATH=$(find ${mavenRepository} -name "*.jar" -printf ':%h/%f')
    export builddir=$TMP/classes
    mkdir -p $builddir

    echo "compiling lock fix workaround"
    javac java/src/lockfix/LockFix.java -cp $CLASSPATH -d $builddir

    echo "compiling clojure sources"
    # On Darwin `clj` tries to create some folder in the home directory...
    ${lib.optionalString stdenv.isDarwin ''
    export HOME=$TMP/home
    mkdir -p $HOME
    ''}
    clj -Scp src:$CLASSPATH:$builddir \
      -J-Djava.awt.headless=true \
      -J-Dclojure.compile.path=$builddir \
      -M -e "(compile (quote ${pname}.core))"

    echo "creating manifest file"
    echo "Main-Class: ${pname}.core" > manifest.txt
    echo "Class-Path: ." >> manifest.txt
    find ${mavenRepository} -name '*.jar' -printf '  %h/%f\n' >> manifest.txt
    cat manifest.txt

    echo "creating fat/uber jar"
    jar cvfm ${name}.jar manifest.txt -C $builddir .

    echo "compiling native image"
    native-image \
      -jar ${name}.jar \
      -H:Name=${pname} \
      ${lib.optionalString stdenv.isDarwin ''-H:-CheckToolchain''} \
      -H:+ReportExceptionStackTraces \
      -J-Dclojure.spec.skip-macros=true \
      -J-Dclojure.compiler.direct-linking=true \
      -J-Djava.awt.headless=true \
      -J-Dclojure.tools.logging.factory=clojure.tools.logging.impl/slf4j-factory \
      --initialize-at-build-time \
      --initialize-at-run-time=sun.font.SunFontManager \
      --initialize-at-run-time=sun.font.StrikeCache \
      --initialize-at-run-time=sun.font.SunLayoutEngine \
      --initialize-at-run-time=sun.font.FontManagerNativeLibrary \
      --initialize-at-run-time=sun.awt.X11GraphicsConfig \
      --initialize-at-run-time=javax.imageio.ImageTypeSpecifier \
      --initialize-at-run-time=sun.java2d.SurfaceData \
      --initialize-at-run-time='com.sun.imageio.plugins.jpeg.JPEG$JCS' \
      --initialize-at-run-time='sun.awt.dnd.SunDropTargetContextPeer$EventDispatcher' \
      --report-unsupported-elements-at-runtime \
      --allow-incomplete-classpath \
      --verbose \
      --no-fallback \
      --no-server
  '';

  installPhase = ''
    install -Dm755 ${pname} $out/detsys-${pname}
  '';
}
