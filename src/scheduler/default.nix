{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  mvn2nix = import (fetchTarball https://github.com/fzakaria/mvn2nix/archive/master.tar.gz) {};
  mavenRepository =
    mvn2nix.buildMavenRepositoryFromLockFile { file = ./mvn2nix-lock.json; };
  inherit (import sources.gitignore {}) gitignoreSource;
in stdenv.mkDerivation rec {
  pname = "scheduler";
  version = "latest";
  name = "${pname}-${version}";
  src = gitignoreSource ./.;

  buildInputs = [ clojure jdk11_headless graalvm11-ce ];

  buildPhase = ''
    export DETSYS_SCHEDULER_VERSION="${lib.commitIdFromGitRepo ./../../.git}"
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
      -J-Dclojure.compile.path=$builddir \
      -M -e "(compile (quote ${pname}.core))"

    echo "creating manifest file"
    echo "Main-Class: ${pname}.core" > manifest.txt
    echo "Class-Path: . resources/" >> manifest.txt
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
      -H:EnableURLProtocols=http,https \
      --enable-all-security-services \
      -H:ResourceConfigurationFiles=${src}/native-image/resource-config.json \
      -H:JNIConfigurationFiles=${src}/native-image/jni-config.json \
      -H:ReflectionConfigurationFiles=${src}/native-image/reflection-config.json \
      -J-Dclojure.spec.skip-macros=true \
      -J-Dclojure.compiler.direct-linking=true \
      -J-Dfile.encoding=UTF-8 \
      --initialize-at-build-time \
      --initialize-at-build-time=org.sqlite.JDBC \
      --initialize-at-build-time=org.sqlite.core.DB$ProgressObserver \
      --initialize-at-build-time=org.sqlite.core.DB \
      --initialize-at-build-time=org.sqlite.core.NativeDB \
      --initialize-at-build-time=org.sqlite.ProgressHandler \
      --initialize-at-build-time=org.sqlite.Function \
      --initialize-at-build-time=org.sqlite.Function$Aggregate \
      --initialize-at-build-time=org.sqlite.Function$Window \
      --report-unsupported-elements-at-runtime \
      --verbose \
      --no-fallback \
      --no-server
  '';

  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 ${pname} $out/bin/detsys-${pname}
  '';
}
