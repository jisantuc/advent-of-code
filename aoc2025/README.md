`aoc2025`
=====

This project was initialized from a Nix flake template.

To get started, replace `project-name` with the name of your project in

* the file name `project-name.cabal` (e.g. rename to `my-executable.cabal`)
* module names in the file contents of `project-name.cabal` (e.g. with `rg -l 'project-name' | xargs sed -i 's/project-name/my-executable/g'`)
* [`ci.yml`](./.github/workflows/ci.yml) test artifact name

Then run a `nix build` to compile everything and run tests. You can verify
success with a `nix run .`, which will call the program defined in `app/Main.hs`.

## Features

* `hspec-discover` for automatic test discover (see `LibSpec` for an example)
* some example dev/test/main dependencies in the `cabal` file
* required plugins for ergonomic use of `hls`, including rename support
* `cabal-install` so you can run tests manually as well for nicer output
