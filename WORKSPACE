workspace(name = "detsys_workspace")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Nix
http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.7.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.7.0.tar.gz"],
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
    revision = "cf7475d2061ac3ada4b226571a4a1bb91420b578",
    sha256 = "a68da1275af117cf314305aeb86cc9f1cacfa68c8b984efd68ac473d3e4bf6f3"
)

nixpkgs_cc_configure(repository = "@nixpkgs//:default.nix")

nixpkgs_python_configure(
    python3_attribute_path = "python3",
    repository = "@nixpkgs//:default.nix",
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
