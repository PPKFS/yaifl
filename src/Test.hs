x = do
  baseKind "object" $ do
    property "name" String_
    property "plural name" (Maybe_ String_)
    property "indefinite article" (Maybe_ String_)
    property NameProperness