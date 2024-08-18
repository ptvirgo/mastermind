# Mastermind Library in Purescript

This is a Purescript implementation of the classic **Mastermind** game.  There's a generic Mastermind library separate from the standard four-colors game, so the library could be used to implement many different guessing game variations (Wordle being a popular example.)

- [Online Demo](https://www.pablovirgo.com/mastermind)

## Build

For the Four Colors demo game:

1. convert MasterMind.Main.defaultMain to Main.main
2. `$ spago bundle --outfile dist/app.js`
3. get your browser to `dist/index.html`, by whatever server you can

It's not saved under Main by default because this appears to cause problems when using the library as an import.  Likely, for a more serious project, it'd make sense to separate the library and game into distinct code bases.
