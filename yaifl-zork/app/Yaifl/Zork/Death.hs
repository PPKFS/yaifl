module Yaifl.Zork.Death where

{-
Chapter 4 - Death and Resurrection
To die saying (reason - text):
  say "[reason][line break]";
  if the player-is-dead is true:
    say "[line break]It takes a talented person to be killed while already dead. YOU are such a talent. Unfortunately, it takes a talented person to deal with it. I am not such a talent. Sorry.[line break]";
    end the story;
    stop;
  if the lucky-flag is false:
    say "Bad luck, huh?[line break]";
  decrease the score by 10;
  say "[line break]    ****  You have died  ****[line break][line break]";
  if the player-deaths is at least 2:
    say "You clearly are a suicidal maniac. We don't allow psychotics in the cave, since they may harm other adventurers. Your remains will be installed in the Land of the Living Dead, where your fellow adventurers may gloat over them.[line break]";
    end the story;
    stop;
  increase the player-deaths by 1;
  if the match-lit is true:
    now the match-lit is false;
    now the match-timer is 0;
  if South Temple is visited:
    say "As you take your last breath, you feel relieved of your burdens. The feeling passes as you find yourself before the gates of Hell, where the spirits jeer at you and deny you entry. Your senses are disturbed. The objects in the dungeon appear indistinct, bleached of color, even unreal.[paragraph break]";
    now the player-is-dead is true;
    now the troll-flag is true;
    now the always-lit-mode is true;
    now the player carries the spirit-glow;
    scatter-possessions;
    move the player to Entrance to Hades;
  otherwise:
    say "Now, let's take a look here...[line break]Well, you probably deserve another chance. I can't quite fix you up completely, but you can't have everything.[paragraph break]";
    scatter-possessions;
    move the player to Forest1.
The spirit-glow is a thing. It is lit. It is undescribed.
Rule for deciding whether all includes the spirit-glow: it does not.
To scatter-possessions:
  now every thing carried by the player is in West-of-House;
  if the player encloses the brass lantern:
    now the brass lantern is in Living Room.

Section - Ghost State Actions
Instead of attacking something when the player-is-dead is true:
  say "All such attacks are vain in your condition."
Instead of taking something when the player-is-dead is true:
  say "Your hand passes through its object."
Instead of removing something from something when the player-is-dead is true:
  say "Your hand passes through its object."
Instead of dropping something when the player-is-dead is true:
  say "You have no possessions."
Instead of throwing something at something when the player-is-dead is true:
  say "You have no possessions."
Instead of taking inventory when the player-is-dead is true:
  say "You have no possessions."
Instead of waiting when the player-is-dead is true:
  say "Might as well. You've got an eternity."
Instead of switching on the brass lantern when the player-is-dead is true:
  say "You need no light to guide you."
Instead of opening something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of closing something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of eating something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of drinking something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of turning something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of burning something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of tying something to something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of rubbing something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of switching on something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of switching off something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of locking something with something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of unlocking something with something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of pushing something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of pulling something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of touching something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of squeezing something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of searching something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of waving something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of putting something on something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of inserting something into something when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of giving something to someone when the player-is-dead is true:
  say "Even such an action is beyond your capabilities."
Instead of doing anything when the player-is-dead is true (this is the ghost state catch-all rule):
  if we are looking or we are examining or we are going or we are praying or we are looking under:
    continue the action;
  say "You can't even do that."
Section - Ghost State Looking
Before looking when the player-is-dead is true:
  if the location is a dark room:
    say "Although there is no light, the room seems dimly illuminated.[line break]";
  say "The room looks strange and unearthly[if the number of visible things in the location is 0].[otherwise] and objects appear indistinct.[end if]";
  say "[line break]".
-}