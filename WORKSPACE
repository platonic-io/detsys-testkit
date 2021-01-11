#load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
#
#http_archive(
#    name = "io_tweag_rules_nixpkgs",
#    strip_prefix = "rules_nixpkgs-0.7.0",
#    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.7.0.tar.gz"],
#    sha256 = "5c80f5ed7b399a857dd04aa81e66efcb012906b268ce607aaf491d8d71f456c8",
#)
#
#load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl",
#     "rules_nixpkgs_dependencies")
#
#rules_nixpkgs_dependencies()
#
#load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository",
#     "nixpkgs_local_repository", "nixpkgs_package")
#
## load("@io_tweag_rules_nixpkgs//nixpkgs:toolchains/go.bzl", "nixpkgs_go_toolchain")
#
#nixpkgs_local_repository(
#    name = "nixpkgs",
#    nix_file_deps = [
#        "//:nix/sources.nix",
#        "//:nix/sources.json",
#        "//:nix/graalvm.nix",
#    ],
#)
#
#nixpkgs_package(
#    name = "checker",
#    nix_file = "//:src/checker/default.nix",
#    repository = "@nixpkgs",
#)
#
#nixpkgs_package(
#    name = "cli",
#    nix_file = "//:src/cli/default.nix",
#    repository = "@nixpkgs",
#)
#
#nixpkgs_package(
#    name = "db",
#    nix_file = "//:src/db/default.nix",
#    repository = "@nixpkgs",
#)
#
#nixpkgs_package(
#    name = "debugger",
#    nix_file = "//:src/debugger/default.nix",
#    repository = "@nixpkgs",
#)
#
#nixpkgs_package(
#    name = "generator",
#    nix_file = "//:src/generator/default.nix",
#    repository = "@nixpkgs",
#)
#
#nixpkgs_package(
#    name = "ldfi",
#    nix_file = "//:src/ldfi/default.nix",
#    repository = "@nixpkgs",
#)
#
#nixpkgs_package(
#    name = "scheduler",
#    nix_file = "//:src/scheduler/default.nix",
#    repository = "@nixpkgs",
#)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

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
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

# gazelle will put stuff here

go_rules_dependencies()

go_register_toolchains(version = "1.15.5")

gazelle_dependencies()