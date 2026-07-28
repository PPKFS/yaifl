module Yaifl.Zork.World.Temple where

{-
TODO
Chapter 15 - Temple, Dome, Egypt, and Hades
Engravings Cave is a dark room. "You have entered a low cave with passages leading northwest and east."
Engravings Cave is in the Underground.
Northwest of Engravings Cave is Round Room. East of Engravings Cave is Dome Room.
The engraved wall is in Engravings Cave. "There are old engravings on the walls here."
Understand "wall" and "engravings" and "inscription" and "old" and "ancient" as the engraved wall.
The description of the engraved wall is "The engravings were incised in the living rock of the cave wall by an unknown hand. They depict, in symbolic form, the beliefs of the ancient Zorkers. Skillfully interwoven with the bas reliefs are excerpts illustrating the major religious tenets of that time. Unfortunately, a later age seems to have considered them blasphemous and just as skillfully excised them."
Dome Room is a dark room. Dome Room is in the Underground.
The description of Dome Room is "You are at the periphery of a large dome, which forms the ceiling of another room below. Protecting you from a precipitous drop is a wooden railing which circles the dome.[if the dome-flag is true][line break]Hanging down from the railing is a rope which ends about ten feet from the floor below.[end if]".
West of Dome Room is Engravings Cave.
The dome-pseudo is a backdrop. The dome-pseudo is in Dome Room and Torch-Room.
The printed name of the dome-pseudo is "dome".
Understand "dome" as the dome-pseudo.
The description of the dome-pseudo is "[if the player is in Dome Room]You are at the periphery of the dome, looking down.[otherwise]The dome forms the ceiling of the room far above you.[end if]"
Instead of kissing the dome-pseudo:
  say "No."
The wooden railing is scenery in Dome Room. Understand "railing" and "rail" and "wooden" as the wooden railing.
The description of the wooden railing is "It's a sturdy wooden railing, suitable for tying things to."
Instead of going down in Dome Room:
  if the dome-flag is true:
    move the player to Torch-Room;
  otherwise:
    say "You cannot go down without fracturing many bones."
After going to Dome Room when the player-is-dead is true:
  say "As you enter the dome you feel a strong pull as if from a wind drawing you over the railing and down.";
  move the player to Torch-Room.
Instead of tying the rope to the wooden railing:
  if the dome-flag is true:
    say "The rope is already tied to it.";
  otherwise:
    now the dome-flag is true;
    now the rope is in Dome Room;
    say "The rope drops over the side and comes within ten feet of the floor."
Rule for writing a paragraph about the rope when the dome-flag is true:
  now the rope is mentioned.
Understand "tie [something] to [something]" as tying it to.
Carry out tying it to:
  say "You can't tie those things together."
Instead of tying the rope to something when the second noun is not the wooden railing:
  say "You can[apostrophe]t tie the rope to that."
Instead of tying-up something:
  if the noun is a person:
    say "The [noun] struggles and you cannot tie him up.";
  otherwise:
    say "Why would you tie up a [noun]?"
Instead of untieing the rope:
  if the dome-flag is true:
    now the dome-flag is false;
    say "The rope is now untied.";
  otherwise:
    say "It is not tied to anything."
Instead of dropping the rope in Dome Room when the dome-flag is false:
  now the rope is in Torch-Room;
  say "The rope drops gently to the floor below."
Instead of taking the rope when the dome-flag is true:
  say "The rope is tied to the railing."
Torch-Room is a dark room. The printed name of Torch-Room is "Torch Room".
The description of Torch-Room is "This is a large room with a prominent doorway leading to a down staircase. Above you is a large dome. Up around the edge of the dome (20 feet up) is a wooden railing. In the center of the room sits a white marble pedestal.[if the dome-flag is true][line break]A piece of rope descends from the railing above, ending some five feet above your head.[end if]".
Torch-Room is in the Underground.
South of Torch-Room is North Temple. Down from Torch-Room is North Temple.
Instead of going up in Torch-Room:
  say "You cannot reach the rope."
The pedestal is a supporter in Torch-Room. The pedestal is scenery.
Understand "pedestal" and "white" and "marble" as the pedestal.
The description of the pedestal is "It's a white marble pedestal."
The torch is a thing on the pedestal. The initial appearance of the torch is "Sitting on the pedestal is a flaming torch, made of ivory."
Understand "torch" and "ivory" and "flaming" as the torch.
The torch is lit. The description of the torch is "The torch is burning."
Instead of switching off the torch:
  say "You nearly burn your hand trying to extinguish the flame."
Instead of pouring the quantity of water on the torch:
  say "The water evaporates before it gets close."
Instead of touching the torch:
  say "You nearly burn your hand."
The treasure-value of the torch is 6.
The point-value of the torch is 14.
North Temple is a dark room. The printed name of North Temple is "Temple". "This is the north end of a large temple. On the east wall is an ancient inscription, probably a prayer in a long-forgotten language. Below the prayer is a staircase leading down. The west wall is solid granite. The exit to the north end of the room is through huge marble pillars."
North Temple is in the Underground.
Down from North Temple is Egypt Room. East of North Temple is Egypt Room. North of North Temple is Torch-Room.
Up from North Temple is Torch-Room.
South of North Temple is South Temple.
The brass bell is in North Temple. "There is a brass bell here."
Understand "bell" and "small" and "brass" as the brass bell.
The description of the brass bell is "It's a small brass bell."
The prayer is scenery in North Temple. Understand "prayer" and "inscription" and "ancient" and "old" as the prayer.
The description of the prayer is "The prayer is inscribed in an ancient script, rarely used today. It seems to be a philippic against small insects, absent-mindedness, and the picking up and dropping of small objects. The final verse consigns trespassers to the land of the dead. All evidence indicates that the beliefs of the ancient Zorkers were obscure."
South Temple is a dark room. The printed name of South Temple is "Altar". "This is the south end of a large temple. In front of you is what appears to be an altar. In one corner is a small hole in the floor which leads into darkness. You probably could not get back up it."
South Temple is in the Underground.
North of South Temple is North Temple.
Instead of going down in South Temple:
  if the player carries the gold coffin:
    say "You haven't a prayer of getting the coffin down there.";
  otherwise:
    move the player to Tiny Cave.
The altar is a supporter in South Temple. The altar is scenery.
Understand "altar" as the altar.
The description of the altar is "It's a massive stone altar."
The pair of candles is on the altar. "On the two ends of the altar are burning candles."
Understand "candles" and "pair" and "burning" as the pair of candles.
The pair of candles is lit.
The description of the pair of candles is "[if the candles-burned-out is true]Alas, there's not much left of the candles. Certainly not enough to burn.[otherwise if the pair of candles is lit]The candles are burning.[otherwise]The candles are out.[end if]".
The black book is on the altar. "On the altar is a large black book, open to page 569."
Understand "book" and "prayer" and "page" and "large" and "black" as the black book.
Instead of closing the black book:
  say "As hard as you try, the book cannot be closed."
Instead of opening the black book:
  say "The book is already open to page 569."
Understand "turn [something]" as turning.
Understand "turn page/pages of/in [something]" as turning.
Instead of turning the black book:
  say "Beside page 569, there is only one other page with any legible printing on it. Most of it is unreadable, but the subject seems to be the banishment of evil. Apparently, certain noises, lights, and prayers are efficacious in this regard."
Instead of burning the black book:
  remove the black book from play;
  die saying "A booming voice says [quotation mark]Wrong, cretin![quotation mark] and you notice that you have turned into a pile of dust. How, I can't imagine."
The description of the black book is "Commandment #12592[paragraph break]Oh ye who go about saying unto each: 'Hello sailor':[line break]Dost thou know the magnitude of thy sin before the gods?[line break]Yea, verily, thou shalt be ground between two stones.[line break]Shall the angry gods cast thy body into the whirlpool?[line break]Surely, thy eye shall be put out with a sharp stick![line break]Even unto the ends of the earth shalt thou wander and[line break]Unto the land of the dead shalt thou be sent at last.[line break]Surely thou shalt repent of thy cunning."
Egypt Room is a dark room. The printed name of Egypt Room is "Egyptian Room". "This is a room which looks like an Egyptian tomb. There is an ascending staircase to the west."
Egypt Room is in the Underground.
West of Egypt Room is North Temple. Up from Egypt Room is North Temple.
The gold coffin is in Egypt Room. "The solid-gold coffin used for the burial of Ramses II is here."
Understand "coffin" and "casket" and "solid" and "gold" as the gold coffin.
The gold coffin is a closed openable container.
The treasure-value of the gold coffin is 15.
The point-value of the gold coffin is 10.
The carrying capacity of the gold coffin is 5.
After opening the gold coffin when the sceptre is in the gold coffin and the sceptre is not handled:
  play the sound of coffin-sfx as sfx;
  say "The gold coffin opens.[line break]";
  say "A sceptre, possibly that of ancient Egypt itself, is in the coffin. The sceptre is ornamented with colored enamel, and tapers to a sharp point." instead.
Rule for writing a paragraph about the gold coffin when the gold coffin is open:
  say "The solid-gold coffin used for the burial of Ramses II is here.[line break]";
  if the sceptre is in the gold coffin and the sceptre is not handled:
    say "A sceptre, possibly that of ancient Egypt itself, is in the coffin. The sceptre is ornamented with colored enamel, and tapers to a sharp point.[line break]";
    now the sceptre is mentioned.
The sceptre is in the gold coffin. "A sceptre, possibly that of ancient Egypt itself, is in the coffin. The sceptre is ornamented with colored enamel, and tapers to a sharp point."
Understand "sceptre" and "scepter" and "sharp" and "egyptian" and "ancient" and "enameled" as the sceptre.
The sceptre is a weapon.
The treasure-value of the sceptre is 6.
The point-value of the sceptre is 4.
Entrance to Hades is a dark room.
The description of Entrance to Hades is "You are outside a large gateway, on which is inscribed[paragraph break]  Abandon every hope all ye who enter here![paragraph break]The gate is open; through it you can see a desolation, with a pile of mangled bodies in one corner. Thousands of voices, lamenting some hideous fate, can be heard.[if the lld-flag is false and the player-is-dead is false][line break]The way through the gate is barred by evil spirits, who jeer at your attempts to pass.[end if]".
Entrance to Hades is in the Underground.
Up from Entrance to Hades is Tiny Cave.
The hades-gate is scenery in Entrance to Hades. The printed name of the hades-gate is "gate".
Understand "gate" and "gates" and "gateway" as the hades-gate when the player is in Entrance to Hades.
The description of the hades-gate is "The gate is protected by an invisible force. It makes your teeth ache to touch it."
Instead of entering the hades-gate: try going south.
Instead of doing anything to the hades-gate:
  unless we are examining or we are entering:
    say "The gate is protected by an invisible force. It makes your teeth ache to touch it." instead.
Instead of going south in Entrance to Hades:
  if the lld-flag is true:
    move the player to Land of the Dead;
  otherwise:
    say "Some invisible force prevents you from passing through the gate."
Instead of going inside in Entrance to Hades:
  if the lld-flag is true:
    move the player to Land of the Dead;
  otherwise:
    say "Some invisible force prevents you from passing through the gate."
The ghosts is scenery in Entrance to Hades. The printed name of the ghosts is "ghosts".
Understand "ghosts" and "spirits" and "fiends" and "force" and "invisible" and "evil" as the ghosts.
The description of the ghosts is "You see a number of ghostly spirits swirling around."
Instead of attacking the ghosts:
  say "How can you attack a spirit with material objects?"
Instead of telling the ghosts about something:
  say "The spirits jeer loudly and ignore you."
Instead of taking or pushing or pulling or touching or rubbing the ghosts:
  say "You seem unable to interact with these spirits."
Exorcising is an action applying to one thing. Understand "exorcise [something]" and "banish [something]" as exorcising.
Carry out exorcising: say "What a bizarre concept!"
Instead of exorcising the ghosts:
  if the player carries the brass bell and the player carries the black book and the player carries the pair of candles:
    say "You must perform the ceremony.";
  otherwise:
    say "You aren[apostrophe]t equipped for an exorcism."
Land of the Dead is a dark room. The printed name of Land of the Dead is "Land of the Dead". "You have entered the Land of the Living Dead. Thousands of lost souls can be heard weeping and moaning. In the corner are stacked the remains of dozens of previous adventurers less fortunate than yourself. A passage exits to the north."
Land of the Dead is in the Underground.
North of Land of the Dead is Entrance to Hades.
The adventurer-bodies is scenery in Land of the Dead. The printed name of the adventurer-bodies is "bodies".
Understand "bodies" and "remains" and "adventurers" and "previous" as the adventurer-bodies.
The description of the adventurer-bodies is "The bodies are piled up in the corner."
Instead of taking the adventurer-bodies: say "A force keeps you from taking the bodies."
Instead of attacking or burning the adventurer-bodies:
  die saying "The voice of the guardian of the dungeon booms out from the darkness, [quotation mark]Your disrespect costs you your life![quotation mark] and places your head on a sharp pole."
The crystal skull is in Land of the Dead. "Lying in one corner of the room is a beautifully carved crystal skull. It appears to be grinning at you rather nastily."
Understand "skull" and "head" and "crystal" as the crystal skull.
The treasure-value of the crystal skull is 10.
The point-value of the crystal skull is 10.
Chapter 16 - Exorcism Ceremony
The xb-flag is a truth state that varies. The xb-flag is false.
The xc-flag is a truth state that varies. The xc-flag is false.
The xb-timer is a number that varies. The xb-timer is 0.
The hot-bell-timer is a number that varies. The hot-bell-timer is 0.
Ringing is an action applying to one thing. Understand "ring [something]" as ringing.
Carry out ringing: say "How, exactly, can you ring that?"
Bell-ringing it with is an action applying to two things. Understand "ring [something] with [something]" as bell-ringing it with.
Carry out bell-ringing it with: say "How, exactly, can you ring that?"
Instead of bell-ringing the red hot brass bell with something:
  if the second noun is the sack or the second noun is the book or the second noun is the bird's nest or the second noun is the pile of leaves or the second noun is the rope:
    say "The [second noun] burns and is consumed.";
    remove the second noun from play;
  otherwise:
    say "The heat from the bell is too intense."
The red hot brass bell is a thing. "On the ground is a red hot bell."
Understand "bell" and "hot" and "red" and "brass" as the red hot brass bell.
The description of the red hot brass bell is "It's a red hot brass bell."
Instead of taking the red hot brass bell:
  say "The bell is very hot and cannot be taken."
Instead of ringing the red hot brass bell:
  say "The bell is too hot to reach."
Instead of rubbing the red hot brass bell:
  say "The bell is too hot to touch."
Instead of touching the red hot brass bell:
  say "The heat from the bell is too intense."
Pouring it on is an action applying to two things. Understand "pour [something] on [something]" as pouring it on.
Carry out pouring it on: say "You can't pour that."
Instead of pouring the quantity of water on the red hot brass bell:
  remove the quantity of water from play;
  say "The water cools the bell and is evaporated.";
  now the hot-bell-timer is 0;
  remove the red hot brass bell from play;
  now the brass bell is in the location of the player.
Instead of ringing the brass bell:
  if the player is in Entrance to Hades and the lld-flag is false:
    now the xb-flag is true;
    now the xb-timer is 6;
    now the hot-bell-timer is 20;
    remove the brass bell from play;
    now the red hot brass bell is in Entrance to Hades;
    play the sound of bell-sfx as sfx;
    say "The bell suddenly becomes red hot and falls to the ground. The wraiths, as if paralyzed, stop their jeering and slowly turn to face you. On their ashen faces, the expression of a long-forgotten terror takes shape.";
    if the player carries the pair of candles:
      say "[line break]In your confusion, the candles drop to the ground (and they are out).";
      now the pair of candles is in Entrance to Hades;
      now the pair of candles is not lit;
  otherwise:
    say "Ding, dong."
Lighting-candles is an action applying to one thing.
Understand "light [something]" as lighting-candles when the noun is the pair of candles.
Instead of lighting-candles the pair of candles:
  if the candles-burned-out is true:
    say "Alas, there's not much left of the candles. Certainly not enough to burn.";
  otherwise if the pair of candles is lit:
    say "The candles are already lit.";
  otherwise if the match-lit is true:
    say "The candles are lit.";
    now the pair of candles is lit;
  otherwise if the player can see the torch:
    say "The heat from the torch is so intense that the candles are vaporized.";
    remove the pair of candles from play;
  otherwise:
    say "You should say what to light them with."
Instead of switching on the pair of candles: say "If you wish to burn the [noun], you should say so."
Instead of inserting the pair of candles into the sack:
  say "That wouldn[apostrophe]t be smart."
Instead of inserting the pair of candles into the bird's nest:
  say "That wouldn[apostrophe]t be smart."
Instead of switching off the pair of candles:
  if the pair of candles is lit:
    now the pair of candles is not lit;
    say "The flame is extinguished.";
    if in darkness:
      say " It's really dark in here....";
  otherwise:
    say "The candles are not lighted."
Every turn when the xb-flag is true and the player is in Entrance to Hades and the xc-flag is false (this is the candle flame power rule):
  if the player carries the pair of candles and the pair of candles is lit:
    now the xc-flag is true;
    say "The flames flicker wildly and appear to dance. The earth beneath your feet trembles, and your legs nearly buckle beneath you. The spirits cower at your unearthly power.[line break]";
    now the xb-timer is 0.
Every turn when the xb-timer > 0 (this is the xb timer rule):
  decrease the xb-timer by 1;
  if the xb-timer is 0 and the xc-flag is false:
    if the player is in Entrance to Hades:
      say "The tension of this ceremony is broken, and the wraiths, amused but shaken at your clumsy attempt, resume their hideous jeering.[line break]";
    now the xb-flag is false.
Every turn when the hot-bell-timer > 0 (this is the hot bell cooling rule):
  decrease the hot-bell-timer by 1;
  if the hot-bell-timer is 0:
    remove the red hot brass bell from play;
    now the brass bell is in Entrance to Hades;
    if the player is in Entrance to Hades:
      say "The bell appears to have cooled down.[line break]".
Instead of reading or examining the black book:
  if the xc-flag is true and the player is in Entrance to Hades and the lld-flag is false:
    now the lld-flag is true;
    remove the ghosts from play;
    play the sound of spirits-sfx as sfx;
    say "Each word of the prayer reverberates through the hall in a deafening confusion. As the last word fades, a voice, loud and commanding, speaks: [quotation mark]Begone, fiends![quotation mark] A heart-stopping scream fills the cavern, and the spirits, sensing a greater power, flee through the walls.";
  otherwise:
    say "[description of the black book]".
-}