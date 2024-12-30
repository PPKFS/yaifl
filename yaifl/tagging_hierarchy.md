
Text

Core

Std
- Actions
- Kinds
- Rulebooks
- EffectHandlers


object
  - thing
  - room

entity
  - taggedentity

taggedobject


Object -> Maybe Thing : getThingMaybe
Object -> Maybe Room : getRoomMaybe

TaggedEntity tag -> m TaggedObject AnyObject tag : TODO
witness -> Object -> TaggedEntity : tag
witness -> o -> TaggedObject o tag : tagObject
o (pre-tagged) -> TaggedEntity : toTag

tagA -> tagB : coerceTag
tagObjA -> tagObjB : TODO

for each property P:
- PTag
- PThing - maybe - /PRoom/PAnyObject
- Taggable P PTag
- [many] Taggable P {ParentTag}
- TaggedAs PThing/PRoom/PAnyObject
- [many] TaggedAs PThing {ParentTag}
