# The Tech

This is a brief overview of some of the pieces that I've used to make this project.

## The Model

Pretty much a clone of `Inform7` semantically, but with less flexibility at runtime -- the world model structure is fully defined at compile time, so adding new objects is easy during play but adding new kinds is not -- in exchange for nicer handling of the world model as a collection of possibly uninteresting objects (rather than a small, but very explicit set of 'interesting' ones).
## Haskell

Mostly it's `optics` (as my lens package of choice), `cleff` (as my effects/mtl replacement package of choice) and `sandwich` (as my test framework of choice).

## Literate Programming

`entangled`

## The Book

`mdBook`
