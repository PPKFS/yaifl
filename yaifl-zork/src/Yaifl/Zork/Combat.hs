module Yaifl.Zork.Combat where

{-
TODO
Chapter 3 - Combat Melee Message System
Section 1 - Combat Variables
The melee-weapon is a thing that varies.
The melee-target is a person that varies.
Section 2 - Hero Attack Messages
Table of Hero Melee Messages
outcome (text)  message (text)
"miss"  "Your [melee-weapon] misses the [melee-target] by an inch."
"miss"  "A good slash, but it misses the [melee-target] by a mile."
"miss"  "You charge, but the [melee-target] jumps nimbly aside."
"miss"  "Clang! Crash! The [melee-target] parries."
"miss"  "A quick stroke, but the [melee-target] is on guard."
"miss"  "A good stroke, but it[apostrophe]s too slow; the [melee-target] dodges."
"unconscious"  "Your [melee-weapon] crashes down, knocking the [melee-target] into dreamland."
"unconscious"  "The [melee-target] is battered into unconsciousness."
"unconscious"  "A furious exchange, and the [melee-target] is knocked out!"
"unconscious"  "The haft of your [melee-weapon] knocks out the [melee-target]."
"unconscious"  "The [melee-target] is knocked out!"
"kill"  "It[apostrophe]s curtains for the [melee-target] as your [melee-weapon] removes his head."
"kill"  "The fatal blow strikes the [melee-target] square in the heart: He dies."
"kill"  "The [melee-target] takes a fatal blow and slumps to the floor dead."
"light-wound"  "The [melee-target] is struck on the arm; blood begins to trickle down."
"light-wound"  "Your [melee-weapon] pinks the [melee-target] on the wrist, but it[apostrophe]s not serious."
"light-wound"  "Your stroke lands, but it was only the flat of the blade."
"light-wound"  "The blow lands, making a shallow gash in the [melee-target][apostrophe]s arm!"
"serious-wound"  "The [melee-target] receives a deep gash in his side."
"serious-wound"  "A savage blow on the thigh! The [melee-target] is stunned but can still fight!"
"serious-wound"  "Slash! Your blow lands! That one hit an artery, it could be serious!"
"serious-wound"  "Slash! Your stroke connects! This could be serious!"
"stagger"  "The [melee-target] is staggered, and drops to his knees."
"stagger"  "The [melee-target] is momentarily disoriented and can[apostrophe]t fight back."
"stagger"  "The force of your blow knocks the [melee-target] back, stunned."
"stagger"  "The [melee-target] is confused and can[apostrophe]t fight back."
"stagger"  "The quickness of your thrust knocks the [melee-target] back, stunned."
"disarm"  "The [melee-target][apostrophe]s weapon is knocked to the floor, leaving him unarmed."
"disarm"  "The [melee-target] is disarmed by a subtle feint past his guard."
Section 3 - Troll Attack Messages
Table of Troll Melee Messages
outcome (text)  message (text)
"miss"  "The troll swings his axe, but it misses."
"miss"  "The troll[apostrophe]s axe barely misses your ear."
"miss"  "The axe sweeps past as you jump aside."
"miss"  "The axe crashes against the rock, throwing sparks!"
"unconscious"  "The flat of the troll[apostrophe]s axe hits you delicately on the head, knocking you out."
"kill"  "The troll neatly removes your head."
"kill"  "The troll[apostrophe]s axe stroke cleaves you from the nave to the chops."
"kill"  "The troll[apostrophe]s axe removes your head."
"light-wound"  "The axe gets you right in the side. Ouch!"
"light-wound"  "The flat of the troll[apostrophe]s axe skins across your forearm."
"light-wound"  "The troll[apostrophe]s swing almost knocks you over as you barely parry in time."
"light-wound"  "The troll swings his axe, and it nicks your arm as you dodge."
"serious-wound"  "The troll charges, and his axe slashes you on your [melee-weapon] arm."
"serious-wound"  "An axe stroke makes a deep wound in your leg."
"serious-wound"  "The troll[apostrophe]s axe swings down, gashing your shoulder."
"stagger"  "The troll hits you with a glancing blow, and you are momentarily stunned."
"stagger"  "The troll swings; the blade turns on your armor but crashes broadside into your head."
"stagger"  "You stagger back under a hail of axe strokes."
"stagger"  "The troll[apostrophe]s mighty blow drops you to your knees."
"disarm"  "The axe hits your [melee-weapon] and knocks it spinning."
"disarm"  "The troll swings, you parry, but the force of his blow knocks your [melee-weapon] away."
"disarm"  "The axe knocks your [melee-weapon] out of your hand. It falls to the floor."
"hesitate"  "The troll hesitates, fingering his axe."
"hesitate"  "The troll scratches his head ruminatively:  Might you be magically protected, he wonders?"
"sitting-duck"  "Conquering his fears, the troll puts you to death."
Section 4 - Thief Attack Messages
Table of Thief Melee Messages
outcome (text)  message (text)
"miss"  "The thief stabs nonchalantly with his stiletto and misses."
"miss"  "You dodge as the thief comes in low."
"miss"  "You parry a lightning thrust, and the thief salutes you with a grim nod."
"miss"  "The thief tries to sneak past your guard, but you twist away."
"unconscious"  "Shifting in the midst of a thrust, the thief knocks you unconscious with the haft of his stiletto."
"unconscious"  "The thief knocks you out."
"kill"  "Finishing you off, the thief inserts his blade into your heart."
"kill"  "The thief comes in from the side, feints, and inserts the blade into your ribs."
"kill"  "The thief bows formally, raises his stiletto, and with a wry grin, ends the battle and your life."
"light-wound"  "A quick thrust pinks your left arm, and blood starts to trickle down."
"light-wound"  "The thief draws blood, raking his stiletto across your arm."
"light-wound"  "The stiletto flashes faster than you can follow, and blood wells from your leg."
"light-wound"  "The thief slowly approaches, strikes like a snake, and leaves you wounded."
"serious-wound"  "The thief strikes like a snake! The resulting wound is serious."
"serious-wound"  "The thief stabs a deep cut in your upper arm."
"serious-wound"  "The stiletto touches your forehead, and the blood obscures your vision."
"serious-wound"  "The thief strikes at your wrist, and suddenly your grip is slippery with blood."
"stagger"  "The butt of his stiletto cracks you on the skull, and you stagger back."
"stagger"  "The thief rams the haft of his blade into your stomach, leaving you out of breath."
"stagger"  "The thief attacks, and you fall back desperately."
"disarm"  "A long, theatrical slash. You catch it on your [melee-weapon], but the thief twists his knife, and the [melee-weapon] goes flying."
"disarm"  "The thief neatly flips your [melee-weapon] out of your hands, and it drops to the floor."
"disarm"  "You parry a low thrust, and your [melee-weapon] slips out of your hand."
"hesitate"  "The thief, a man of superior breeding, pauses for a moment to consider the propriety of finishing you off."
"hesitate"  "The thief amuses himself by searching your pockets."
"hesitate"  "The thief entertains himself by rifling your pack."
"sitting-duck"  "The thief, forgetting his essentially genteel upbringing, cuts your throat."
"sitting-duck"  "The thief, a pragmatist, dispatches you as a threat to his livelihood."
Section 5 - Cyclops Attack Messages
Table of Cyclops Melee Messages
outcome (text)  message (text)
"miss"  "The Cyclops misses, but the backwash almost knocks you over."
"miss"  "The Cyclops rushes you, but runs into the wall."
"unconscious"  "The Cyclops sends you crashing to the floor, unconscious."
"kill"  "The Cyclops breaks your neck with a massive smash."
"light-wound"  "A quick punch, but it was only a glancing blow."
"light-wound"  "A glancing blow from the Cyclops[apostrophe] fist."
"serious-wound"  "The monster smashes his huge fist into your chest, breaking several ribs."
"serious-wound"  "The Cyclops almost knocks the wind out of you with a quick punch."
"stagger"  "The Cyclops lands a punch that knocks the wind out of you."
"stagger"  "Heedless of your weapons, the Cyclops tosses you against the rock wall of the room."
"disarm"  "The Cyclops grabs your [melee-weapon], tastes it, and throws it to the ground in disgust."
"disarm"  "The monster grabs you on the wrist, squeezes, and you drop your [melee-weapon] in pain."
"hesitate"  "The Cyclops seems unable to decide whether to broil or stew his dinner."
"sitting-duck"  "The Cyclops, no sportsman, dispatches his unconscious victim."
Section 6 - Melee Message Selection
To print hero melee for (O - text):
  let N be 0;
  repeat through the Table of Hero Melee Messages:
    if the outcome entry is O:
      increase N by 1;
  if N > 0:
    let R be a random number between 1 and N;
    let C be 0;
    let done be false;
    repeat through the Table of Hero Melee Messages:
      if done is false and the outcome entry is O:
        increase C by 1;
        if C is R:
          say "[message entry]";
          now done is true.
To print troll melee for (O - text):
  let N be 0;
  repeat through the Table of Troll Melee Messages:
    if the outcome entry is O:
      increase N by 1;
  if N > 0:
    let R be a random number between 1 and N;
    let C be 0;
    let done be false;
    repeat through the Table of Troll Melee Messages:
      if done is false and the outcome entry is O:
        increase C by 1;
        if C is R:
          say "[message entry]";
          now done is true.
To print thief melee for (O - text):
  let N be 0;
  repeat through the Table of Thief Melee Messages:
    if the outcome entry is O:
      increase N by 1;
  if N > 0:
    let R be a random number between 1 and N;
    let C be 0;
    let done be false;
    repeat through the Table of Thief Melee Messages:
      if done is false and the outcome entry is O:
        increase C by 1;
        if C is R:
          say "[message entry]";
          now done is true.
To print cyclops melee for (O - text):
  let N be 0;
  repeat through the Table of Cyclops Melee Messages:
    if the outcome entry is O:
      increase N by 1;
  if N > 0:
    let R be a random number between 1 and N;
    let C be 0;
    let done be false;
    repeat through the Table of Cyclops Melee Messages:
      if done is false and the outcome entry is O:
        increase C by 1;
        if C is R:
          say "[message entry]";
          now done is true.

Section 7 - Villain Death Message
To say sinister-black-fog for (V - a person):
  say "Almost as soon as the [V] breathes his last breath, a cloud of sinister black fog envelops him, and when the fog lifts, the carcass has disappeared."
-}