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

I'd like to switch to GHC 9.2, but currently HLS doesn't like `cleff-plugin`.

## Library stanza
```cabal file=yaifl.cabal
library
  import:          common-options
  hs-source-dirs:  src
  exposed-modules:
    Yaifl

    Yaifl.Core.Actions.Action
    Yaifl.Core.Actions.Activity
    Yaifl.Core.Actions.Parser
    Yaifl.Core.Common
    Yaifl.Core.Directions
    Yaifl.Core.Logger
    Yaifl.Core.Objects.Create
    Yaifl.Core.Objects.Dynamic
    Yaifl.Core.Objects.Move
    Yaifl.Core.Objects.Object
    Yaifl.Core.Objects.ObjectData
    Yaifl.Core.Objects.Query
    Yaifl.Core.Objects.Room
    Yaifl.Core.Objects.Specifics
    Yaifl.Core.Properties.Enclosing
    Yaifl.Core.Properties.Property
    Yaifl.Core.Properties.Query
    Yaifl.Core.Properties.TH

    Yaifl.Core.Rulebooks.ActionProcessing
    Yaifl.Core.Rulebooks.Args
    Yaifl.Core.Rulebooks.Run
    Yaifl.Core.Rulebooks.Rule
    Yaifl.Core.Rulebooks.Rulebook
    Yaifl.Core.Rulebooks.WhenPlayBegins

    Yaifl.Core.Say
    Yaifl.Core.World

    Yaifl.Lamp.Properties.Openable
    Yaifl.Lamp.Properties.Container
    --Yaifl.Properties.Supporter

    --Yaifl.ActivityCollection
    --Yaifl.Actions.Going
    --Yaifl.Actions.Looking
    --Yaifl.Activities.Activity
    --Yaifl.Activities.ChoosingNotableLocaleObjects
    --Yaifl.Activities.DescribingLocale
    --Yaifl.Activities.PrintingADarkRoom
    --Yaifl.Activities.PrintingDescriptionOfADarkRoom
    --Yaifl.Activities.PrintingLocaleParagraphAbout
    --Yaifl.Activities.PrintingNameOfSomething
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
