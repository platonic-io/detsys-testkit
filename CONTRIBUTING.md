### Contributing

This project is still in its early stages and will likely change in breaking
ways.

Please see the [`ROADMAP.md`](ROADMAP.md) and the [issue
tracker](https://github.com/symbiont-io/detsys-testkit/issues) for the general
direction of where we are heading.

We'd like to encourage experimentation, extensions and rewrites but do open a
ticket beforehand to avoid wasting effort.

We roughly follow the angular commit message
[format](https://github.com/conventional-changelog/conventional-changelog/blob/a5505865ff3dd710cf757f50530e73ef0ca641da/conventions/angular.md),
please have a look at `git log` and try to mimic along.

The commit messages are used to generate our [`CHANGELOG.md`](CHANGELOG.md)
using [`tools/generate-changelog`](tools/generate-changelog). To see what's new
since some release, e.g. `v$TAG`, run `./tools/genenerate-changelog v$TAG`.

If you want to make a new release, first update the `CHANGELOG.md` with the
output from the above step and replacing `$TAG` with the last version and `HEAD`
with the next version, then tag and push the new version with:

```bash
    export NEW_VERSION=X.Y.Z # Try to follow semantic versioning.

    export LAST_VERSION="$(git tag | sort | tail -1)"
    ./tools/generate-changelog "${LAST_VERSION}" | \
        sed "s/\[HEAD\]/\[v${NEW_VERSION}\]/" | \
        sed "s/\.\.\.HEAD/...v${NEW_VERSION}/" > /tmp/NEW_CHANGELOG.md
    cat CHANGELOG.md >> /tmp/NEW_CHANGELOG.md
    mv /tmp/NEW_CHANGELOG.md CHANGELOG.md

    git diff # Check that everything looks alright.
    git checkout -b update-changelog-"${NEW_VERSION}"
    git add CHANGELOG.md
    git commit -m "docs: update changelog for release v${NEW_VERSION}"
    gh pr create # Or create a PR via the web UI.

    # Merge the PR.

    git checkout main
    git pull
    git branch -d update-changelog-"${NEW_VERSION}"

    git tag -a "v${NEW_VERSION}" -m "tag: v${NEW_VERSION}"
    git push origin "v${NEW_VERSION}"
```
