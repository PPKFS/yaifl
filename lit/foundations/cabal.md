# Cabal, Extensions, Dependencies

This is probably a pretty dull page, so if you only care about the interesting stuff
you can skip it. But I wanted to do this literate programming thing properly, and also
it ensures I know what's going on in my `.cabal` files.

## Metadata

Everything here is fairly standard project information.

```cabal file=yaifl.cabal header="1"
cabal-version:   3.0
name:            yaifl
version:         0.0.0.1
synopsis:        Yet another interactive fiction library.
description:     Yet another interactive fiction library.
homepage:        https://github.com/PPKFS/yaifl
bug-reports:     https://github.com/PPKFS/yaifl/issues
license:         MIT
author:          Avery
maintainer:      Avery <thecommunistduck@hotmail.co.uk>
copyright:       2022 Avery
category:        Game Development
build-type:      Simple
tested-with: GHC == 9.0.2

source-repository head
  type:     git
  location: https://github.com/PPKFS/yaifl.git
```

## Dependencies

This is a pretty standard dependency list.

```cabal file=yaifl.cabal
common common-options
  build-depends:
      base
    , containers
    , template-haskell
    , text
    , text-display
    , solitude
    , cleff
    , cleff-plugin
    , time
```

I still have no idea why the first 4 of these aren't in `base`. `solitude` is my personal prelude, which is
mostly re-exports of the excellent [relude](https://hackage.haskell.org/package/relude) alternative prelude and
also some `optics`-lens things. 

```admonish question "Why optics over lens?"

- No idea, I just wanted to.
- The error messages and the explicit `AffineTraversal` you get from combining a `Lens` and a `Prism` are cool though.
```

```cabal file=yaifl.cabal
    , display
    , prettyprinter
    , prettyprinter-ansi-terminal
```

`display` is for being *technically* lawful with `Show` instances when it comes to logging and for making pretty error messages. I would also like to re-add `chapelure` but I've not quite found the use-case (maybe in the test suite). `prettyprinter` gives nicer string formatting options.

```cabal file=yaifl.cabal
    , aeson
    , katip
    , enummapset
    , haskell-src-meta
    , haskell-src-exts
```

- `enummapset` is a nice set of wrappers for using `Enum` keys in `IntMap`s for better performance (i.e. `Entity`).
- `haskell-src-*` I use because writing well-formed TH is hard, and I wanted to just write Haskell strings with
text substitutions in. 

- `katip` is only currently used because it has excellent time formatting, but it's a bit heavy for that purpose.

## GHC extensions

```cabal file=yaifl.cabal
  ghc-options:
    -Wall -Wcompat -Widentities -Wredundant-constraints 
    -fhide-source-paths -Wno-unused-top-binds
    -Wmissing-deriving-strategies -O2 -flate-specialise
    -fspecialise-aggressively -fprint-potential-instances
    -fno-warn-unused-do-bind -haddock -fwrite-ide-info
    -fplugin=Cleff.Plugin
  default-language:   Haskell2010
  default-extensions:
    NoImplicitPrelude
    BlockArguments
    DataKinds
    DerivingStrategies
    FunctionalDependencies
    LambdaCase
    MultiWayIf
    OverloadedStrings
    TypeFamilies
    TypeApplications
    ConstraintKinds
    FlexibleInstances
    FlexibleContexts
    GeneralisedNewtypeDeriving
    DeriveGeneric
    DeriveTraversable
    StandaloneDeriving
    RankNTypes
    ScopedTypeVariables
    BangPatterns
    GADTs
    TypeOperators
    DerivingVia
```

We enable a whole bunch of options and extensions. Notably `NoImplicitPrelude` makes it easier than fiddling with
mixins for using `solitude` over `Prelude`, `BlockArguments` for my love of using inline `do` blocks, and `TypeFamilies`
because I like to try and be smarter than I am.

## Library stanza
```cabal file=yaifl.cabal
library
  import:          common-options
  hs-source-dirs:  src
  exposed-modules:
    Yaifl

    --Yaifl.Actions.Action
    --Yaifl.Actions.Going
    --Yaifl.Actions.Looking

    --Yaifl.Activities.Activity
    --Yaifl.Activities.ChoosingNotableLocaleObjects
    --Yaifl.Activities.DescribingLocale
    --Yaifl.Activities.PrintingADarkRoom
    --Yaifl.Activities.PrintingDescriptionOfADarkRoom
    --Yaifl.Activities.PrintingLocaleParagraphAbout
    --Yaifl.Activities.PrintingNameOfSomething

    --Yaifl.ActivityCollection
    Yaifl.Common
    Yaifl.Directions
    Yaifl.Logger

    Yaifl.Objects.Create
    Yaifl.Objects.Dynamic
    Yaifl.Objects.Move
    Yaifl.Objects.Object
    Yaifl.Objects.ObjectData
    Yaifl.Objects.Query
    Yaifl.Objects.Room
    Yaifl.Objects.Specifics

    Yaifl.Properties.Container
    Yaifl.Properties.Enclosing
    Yaifl.Properties.Openable
    Yaifl.Properties.Property
    Yaifl.Properties.Query
    --Yaifl.Properties.Supporter
    Yaifl.Properties.TH

    --Yaifl.Rulebooks.ActionProcessing
    --Yaifl.Rulebooks.Args
    --Yaifl.Rulebooks.Rulebook
    --Yaifl.Rulebooks.WhenPlayBegins

    Yaifl.Say
    Yaifl.World
```

## Test stanza

- `neat-interpolation` makes wrapped raw string quasi-quotes better, which is important given
how many room descriptions are very long lines of text. 
- `sandwich` is a really sweet looking testing library so I wanted to try it.
- `conduit` I needed because of `MonadThrow` constraints for `sandwich`'s `shouldBe`.
```cabal file=yaifl.cabal
test-suite yaifl-test
  import:             common-options
  type:               exitcode-stdio-1.0
  hs-source-dirs:     test
  main-is:            Spec.hs
  build-depends:
    , sandwich
    , conduit
    , yaifl
  ghc-options:        -threaded -rtsopts -with-rtsopts=-N
  default-extensions:
    QuasiQuotes
    TemplateHaskell

  other-modules:
    Yaifl.Test.Chapter3.Bic
    Yaifl.Test.Chapter3.Common
    --Yaifl.Test.Chapter3.Verbosity
    Yaifl.Test.Common
```
