## Scheduler

### Testing, building and running

To run the tests:

```bash
clj -A:test
```

Build and run from command-line:

```bash
clj -m schduler.core
```

Build and run uberjar from command-line:

```bash
clj -A:uberjar
java -cp target/scheduler.jar clojure.main -m scheduler.core
```

Build and run native image via GraalVM:

```bash
clj -A:native-image
./target/scheduler
```

### Static analysis

Check and fix code formatting:

```bash
clj -A:cljfmt
```

Check linting:

```bash
clj -A:eastwood
clj -A:clj-kondo
```

Check if any dependencies are outdated:

```bash
clj -A:outdated
```

### Building with `nix`

If the dependencies have changed, then first run:

```bash
clj -Spom
nix run -f https://github.com/fzakaria/mvn2nix/archive/master.tar.gz \
    --command mvn2nix \
    --repositories=https://repo1.maven.org/maven2/ https://repo.clojars.org \
    > mvn2nix-lock.json
```

Otherwise, or afterwards, we can then build and run with:

```bash
nix-build
./result/bin/scheduler-0.1.0
```
