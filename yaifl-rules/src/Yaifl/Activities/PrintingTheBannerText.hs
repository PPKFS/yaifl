module Yaifl.Activities.PrintingTheBannerText
  ( printingTheBannerTextImpl
  , WithPrintingTheBannerText

  ) where

import Yaifl.Prelude
import Yaifl.Activity
import Yaifl.Rulebook
import Yaifl.Text.SayableValue (SayableValue)
import Yaifl.WorldModel
import Yaifl.Metadata
import Yaifl.Effects.Print
import qualified Data.Text as T

type WithPrintingTheBannerText wm =
  ( WithActivity "printingTheBannerText" wm () () ()
  , SayableValue (WMText wm) wm
  )

type PrintingTheBannerTextRule wm = ActivityRule wm () () ()

printingTheBannerTextImpl :: Activity wm () () ()
printingTheBannerTextImpl = makeActivity "printing the banner text" [makeRule "printing the banner text" [] (const $ sayIntroText >> rulePass) ]

sayIntroText ::
  State (Metadata wm) :> es
  => Print  :> es
  => Eff es ()
sayIntroText = do
  setStyle (Just (colour (Colour 0xff147421) <> bold))
  t <- use #title
  printLn $ introText t
  setStyle Nothing
  pass

introText ::
  Text
  -> Text
introText w = fold
  [ longBorder <> "\n"
  , shortBorder <> " " <> w <> " " <> shortBorder <> "\n"
  , longBorder
  ]
  where
    shortBorder = "-----"
    longBorder = mconcat $ replicate
      (2 * T.length shortBorder + T.length w + 2) "-"