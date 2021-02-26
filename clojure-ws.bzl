load("@rules_jvm_external//:defs.bzl", "maven_install")
load("@rules_jvm_external//:specs.bzl", "maven")

def clojure_ws():
    maven_install(
        artifacts = [
            maven.artifact(
                "org.clojure",
                "clojure",
                "1.10.2",
                exclusions = [
                    "org.clojure:core.specs.alpha",
                    "org.clojure:spec.alpha",
                ],
            ),
            maven.artifact(
                "org.clojure",
                "spec.alpha",
                "0.2.194",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            maven.artifact(
                "org.clojure",
                "core.specs.alpha",
                "0.2.56",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            maven.artifact(
                "ring",
                "ring-core",
                "1.8.1",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            maven.artifact(
                "ring",
                "ring-devel",
                "1.8.1",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            maven.artifact(
                "ring",
                "ring-defaults",
                "0.3.2",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            maven.artifact(
                "ring",
                "ring-jetty-adapter",
                "1.8.1",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            "clj-http:clj-http:3.11.0",
            maven.artifact(
                "clj-http-fake",
                "clj-http-fake",
                "1.0.3",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            maven.artifact(
                "org.clojure",
                "data.generators",
                "1.0.0",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            "shams:priority-queue:0.1.2",
            maven.artifact(
                "seancorfield",
                "next.jdbc",
                "1.1.582",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            "org.xerial:sqlite-jdbc:3.32.3.2",
            maven.artifact(
                "com.taoensso",
                "timbre",
                "4.10.0",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
            "metosin:jsonista:0.2.7",
            maven.artifact(
                "gnl",
                "ghostwheel",
                "0.3.9",
                exclusions = [
                    "org.clojure:clojure",
                ],
            ),
        ],
        repositories = [
            "https://repo1.maven.org/maven2",
            "https://repo.clojars.org",
        ],
        fetch_sources = True,
    )
