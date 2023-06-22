# Mastermind Library in Purescript

This is a Purescript implementation of the classic **Mastermind** game.  There's a generic Mastermind library separate from the standard four-colors game, so the library could be used to implement many different guessing game variations (Wordle being a popular example.)

- [Online Demo](https://www.pablovirgo.com/mastermind)

## Build

    $ spago bundle-app -t dist/app.js

Then get your browser to `dist/index.html`, by whatever server you can.

## Code Commentary

This is probably most useful as one example of how a Purescript/Halogen app can be organized.

- There's a generic Mastermind module that establishes type classes and default logic for a Mastermind game.
- There's a FourColors library that includes:
    1. A distinct core module with base types and class definitions.
    2. Separate component modules for the major feautures of a board.
    3. A single Game module & component that puts the sub compoenents together.

I could imagine the `FourColors.Game` module being renamed to `FourColors.Main`.  Purescript is relatively obscure, so there are probably conventions I'm unaware of, but this should be a good example of how code can be organized into reusable and modular parts.

On future projects, it might be handy to separate functions that operate on component state from their components, in order to allow unit testing.  Purescripts strong types make it easy to get away with being slightly lazy on that front.
