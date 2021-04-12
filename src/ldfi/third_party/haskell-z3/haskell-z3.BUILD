load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_library",
)

load(
    "@stackage//:packages.bzl",
    "packages",
)

cc_library(
    name = "libz3",
    srcs = ["@z3.lib//:lib"],
    hdrs = ["@z3.dev//:include"],
    strip_include_prefix = "/include",
    linkstatic = 1,
)

haskell_cabal_library(
    name = "z3",
    deps = packages["z3"].deps + [":libz3"],
    srcs = glob([
        "**",
    ]),
    version = "408.2",
    visibility = [
        "//visibility:public",
    ],
)
