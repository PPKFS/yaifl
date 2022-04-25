# Foundations

This section covers the groundwork for getting everything to fit together.

- [Cabal, Extensions, Dependencies](foundations/cabal.md) documents the Cabal files and other various configs.
- [Project Architecture](foundations/architecture.md) describes a higher-level structure of the library as a whole,
and how we apply the Three-Layer Cake principle to try and keep the design clean.
- [wm :: WorldModel](foundations/worldmodel.md) covers the `wm` type parameter that appears everywhere in the code.
- [Entities, Stores and Objects](world/entities.md) covers the most basic building blocks of the library. This is put early on
because they permeate everything.
- [Effects](foundations/effects.md) documents the effect stack.
- [The World State](world/state.md) is the glue that holds everything together, as the entire program is basically an overly
convoluted wrapper around `State (World wm) a`.
- [Construction and Execution](foundations/construction.md) is about building a world and then running the game itself.
