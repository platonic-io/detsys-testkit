## Scheduler

To run the tests:

```bash
clj -A:test
```

Build and run uberjar:

```bash
clj -Auberjar
java -cp target/scheduler-0.0.0.jar clojure.main -m scheduler.core
```
