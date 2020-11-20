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
