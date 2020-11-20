FROM ubuntu:20.04 as builder

# Install compile-time dependencies.
RUN apt-get update && \
    apt-get install -y \
    build-essential \
    curl \
    openjdk-11-jdk-headless \
    zlib1g-dev

WORKDIR /app

# Install GraalVM.
RUN curl -sSLO https://github.com/graalvm/graalvm-ce-dev-builds/releases/download/21.0.0-dev-20201027_0217/graalvm-ce-java11-linux-amd64-dev.tar.gz && \
    tar xf graalvm-ce-java11-linux-amd64-dev.tar.gz && \
    /app/graalvm-ce-java11-21.0.0-dev/bin/gu install native-image

ENV GRAALVM_HOME /app/graalvm-ce-java11-21.0.0-dev

# Install Clojure.
RUN curl -sSLO https://download.clojure.org/install/linux-install-1.10.1.727.sh && \
    chmod 755 linux-install-1.10.1.727.sh && \
    ./linux-install-1.10.1.727.sh

# Copy in the source code.
COPY deps.edn /app/
COPY ./src/ /app/src
COPY ./java/ /app/java

# Install dependencies.
RUN clojure -e ""

# Install monitor bug fix.
RUN javac java/src/lockfix/LockFix.java -cp \
    /root/.m2/repository/org/clojure/clojure/1.10.2-alpha2/clojure-1.10.2-alpha2.jar

# Add a non-root user.
ARG user
ARG uid
ARG gid
RUN addgroup --gid $gid $user \
	  && adduser --uid $uid --ingroup $user --disabled-password $user

RUN chown $user:$user -R /app
USER $user

# Compile the native image via GraalVM.
RUN cd /app && \
    mkdir target && \
    clojure -A:native-image

FROM ubuntu:20.04

# Install run-time dependencies.
RUN apt-get update && \
    apt-get install -y \
    sqlite3

COPY --from=builder /app/target/checker .

ENTRYPOINT ["./checker"]