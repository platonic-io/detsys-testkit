load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.7.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.7.0.tar.gz"],
    sha256 = "5c80f5ed7b399a857dd04aa81e66efcb012906b268ce607aaf491d8d71f456c8",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl",
     "rules_nixpkgs_dependencies")

rules_nixpkgs_dependencies()

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository",
     "nixpkgs_local_repository", "nixpkgs_package")

# load("@io_tweag_rules_nixpkgs//nixpkgs:toolchains/go.bzl", "nixpkgs_go_toolchain")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file_deps = [
        "//:nix/sources.nix",
        "//:nix/sources.json",
        "//:nix/graalvm.nix",
    ],
)

nixpkgs_package(
    name = "checker",
    nix_file = "//:src/checker/default.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "cli",
    nix_file = "//:src/cli/default.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "db",
    nix_file = "//:src/db/default.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "debugger",
    nix_file = "//:src/debugger/default.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "generator",
    nix_file = "//:src/generator/default.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "ldfi",
    nix_file = "//:src/ldfi/default.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "scheduler",
    nix_file = "//:src/scheduler/default.nix",
    repository = "@nixpkgs",
)
