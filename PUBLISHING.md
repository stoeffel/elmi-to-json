# How to publish releases of elmi-to-json.cabal


## Preparation

1. Bump version in package.yaml, package.json, and package-lock.json
1. Create a tag for the new version. `git tag -s <version> -m <version>`
1. Push the tag. `git push && git push origin <version>`
1. Wait for [CI to successfully build the tag](https://travis-ci.org/stoeffel/elmi-to-json/builds), this will create a new github release.


## Publishing

1. Check [release page](https://github.com/stoeffel/elmi-to-json/releases) on github if both (TODO Windows) OSX and Linux binaries are there.
2. Write the release notes on github.
3. Publish new npm version
```
npm publish
```
