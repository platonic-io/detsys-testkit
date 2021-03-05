workspace(name = "detsys_workspace")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Nix
http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.8.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.8.0.tar.gz"],
    sha256 = "7aee35c95251c1751e765f7da09c3bb096d41e6d6dca3c72544781a5573be4aa"
)

load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")
rules_nixpkgs_dependencies()

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_git_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

# Same revision as we pinned with niv in nix/sources.json. For ticket to add
# niv support, see https://github.com/tweag/rules_nixpkgs/issues/127 .
nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "772406c2a4e22a85620854056a4cd02856fa10f0",
    sha256 = "4e3429bc83182b4dc49a554a44067bd9453bd6c0827b08948b324d8a4bb3dea3",
)

nixpkgs_cc_configure(
    name = "nixpkgs_config_cc",
    repository = "@nixpkgs//:default.nix",
)

nixpkgs_package(
    name = "z3.dev",
    repositories = { "nixpkgs": "@nixpkgs//:default.nix" }
)

nixpkgs_package(
    name = "z3.lib",
    repositories = { "nixpkgs": "@nixpkgs//:default.nix" }
)

nixpkgs_package(
    name = "zlib.dev",
    repositories = { "nixpkgs": "@nixpkgs//:default.nix" }
)

nixpkgs_package(
    name = "zlib.out",
    repositories = { "nixpkgs": "@nixpkgs//:default.nix" }
)

# Python
nixpkgs_python_configure(
    python3_attribute_path = "python3",
    repository = "@nixpkgs//:default.nix",
)

http_archive(
    name = "rules_python",
    url = "https://github.com/bazelbuild/rules_python/releases/download/0.1.0/rules_python-0.1.0.tar.gz",
    sha256 = "b6d46438523a3ec0f3cead544190ee13223a52f6a6765a29eae7b7cc24cc83a0",
)

load("@rules_python//python:pip.bzl", "pip_install")

pip_install(requirements = "//src/ldfi:requirements.txt")

# Haskell
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-cf1300cffed8b786420a77d67e1b1481232f84c7",
    urls = ["https://github.com/tweag/rules_haskell/archive/cf1300cffed8b786420a77d67e1b1481232f84c7.tar.gz"],
    sha256 = "0787d2093d160b2c3ce3196c3a00c5fddcf1ec132f58f8dc0fd842160e1f9bbc",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc8103",
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "8.10.3",
)

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains(version = "8.10.3")

http_archive(
    name = "haskell_z3",
    build_file = "//src/ldfi2/third_party/haskell-z3:haskell-z3.BUILD",
    strip_prefix = "haskell-z3-e8af470c0e6045d063f2361719dfac488e5476bd",
    sha256 = "5ce97d4315855d2ec4abdd0f7c2404225d3abfbd80a6c2a9e1ff8de62c8a5cc2",
    urls = [
        "https://github.com/IagoAbal/haskell-z3/archive/e8af470c0e6045d063f2361719dfac488e5476bd.tar.gz",
    ],
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "containers",
        "filepath",
        "mtl",
        "optparse-generic",
        "template-haskell",
        "QuickCheck",
        "sqlite-simple",
        "text",

        # z3-haskell dependencies
        "transformers",

    ],
    snapshot = "lts-17.2",
    extra_deps = {"z3": ["@z3.dev//:include", "@z3.lib//:lib"]},
    vendored_packages = {
        "z3": "@haskell_z3//:z3",
    }
)

# Golang
http_archive(
    name = "io_bazel_rules_go",
    sha256 = "6f111c57fd50baf5b8ee9d63024874dd2a014b069426156c55adbf6d3d22cb7b",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.25.0/rules_go-v0.25.0.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.25.0/rules_go-v0.25.0.tar.gz",
    ],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "b85f48fa105c4403326e9525ad2b2cc437babaa6e15a3fc0b1dbab0ab064bc7c",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.22.2/bazel-gazelle-v0.22.2.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.22.2/bazel-gazelle-v0.22.2.tar.gz",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

# Use go from nixpkgs.
load("@io_tweag_rules_nixpkgs//nixpkgs:toolchains/go.bzl", "nixpkgs_go_configure")
nixpkgs_go_configure(repository = "@nixpkgs//:default.nix")

go_rules_dependencies()

# gazelle will put stuff here

load("//:gazelle-ws.bzl", "gazelle_ws")

# gazelle:repository_macro gazelle-ws.bzl%gazelle_ws
gazelle_ws()

gazelle_dependencies()

# Clojure
RULES_JVM_EXTERNAL_TAG = "4.0"
RULES_JVM_EXTERNAL_SHA = "31701ad93dbfe544d597dbe62c9a1fdd76d81d8a9150c2bf1ecf928ecdf97169"

http_archive(
    name = "rules_jvm_external",
    strip_prefix = "rules_jvm_external-%s" % RULES_JVM_EXTERNAL_TAG,
    sha256 = RULES_JVM_EXTERNAL_SHA,
    url = "https://github.com/bazelbuild/rules_jvm_external/archive/%s.zip" % RULES_JVM_EXTERNAL_TAG)

load("@rules_jvm_external//:defs.bzl", "maven_install")

git_repository(
    name = "rules_clojure",
    commit = "dbcaaa516e8cfcb32e51e80a838cf2be6bf93093",
    remote = "https://github.com/stevana/rules_clojure.git",
)

load("@rules_clojure//:repositories.bzl", "rules_clojure_dependencies")
load("@rules_clojure//:toolchains.bzl", "rules_clojure_toolchains")

rules_clojure_dependencies()
rules_clojure_toolchains()

load("//:clojure-ws.bzl", "clojure_ws")

clojure_ws()

# GraalVM
http_archive(
    name = "rules_graal",
    sha256 = "fc581a429f558ad7d8a9b68942370505654715b468905cb6fda4b0bf7d762871",
    strip_prefix = "rules_graal-d7ebee5ba46a836cff09b5e676b28ac89b2243bd",
    urls = [
        "https://github.com/BraeWebb/rules_graal/archive/d7ebee5ba46a836cff09b5e676b28ac89b2243bd.zip",
    ],
)

load("@rules_graal//graal:graal_bindist.bzl", "graal_bindist_repository")

graal_bindist_repository(
    name = "graal",
    java_version = "11",
    version = "20.2.0",
)
