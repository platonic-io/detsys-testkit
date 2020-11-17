## Checker

Build and run native image via GraalVM:

  1. Download the approprate
     [graalvm-ce-java*](https://github.com/graalvm/graalvm-ce-builds/releases)
     tarball;

  2. Extract the tarball somewhere;

  3. Set your `GRAALVM_HOME` environment variable to point to where you
     extracted the tarball;

  4. Build and run the checker using:

```bash
clj -A:native-image
./target/checker
```

### Build docker image

From the same directory as this README is in, do:

```bash
docker build \
  --build-arg user=$(whoami) \
  --build-arg uid=$(id -u) \
  --build-arg gid=$(id -g) \
  -t detsys/checker -f docker/Dockerfile .
```
