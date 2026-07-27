module Yaifl.Zork.Lamps where

{-

Chapter 6 - Lamp Timer System
The lamp-turns is a number that varies. The lamp-turns is 0.
The lamp-stage is a number that varies. The lamp-stage is 0.
The lamp-burned-out is a truth state that varies. The lamp-burned-out is false.
Every turn when the brass lantern is lit:
  increase the lamp-turns by 1;
  if the lamp-stage is 0 and the lamp-turns is at least 200:
    now the lamp-stage is 1;
    if the player can see the brass lantern:
      say "The lamp appears a bit dimmer.[line break]";
  if the lamp-stage is 1 and the lamp-turns is at least 300:
    now the lamp-stage is 2;
    if the player can see the brass lantern:
      say "The lamp is definitely dimmer now.[line break]";
  if the lamp-stage is 2 and the lamp-turns is at least 370:
    now the lamp-stage is 3;
    if the player can see the brass lantern:
      say "The lamp is nearly out.[line break]";
  if the lamp-turns is at least 385:
    now the lamp-burned-out is true;
    now the brass lantern is not lit;
    now the lamp-stage is 4;
    if the player can see the brass lantern:
      say "You'd better have more light than from the brass lantern.[line break]".
Chapter 6a - Candle Timer System
The candle-turns is a number that varies. The candle-turns is 0.
The candle-stage is a number that varies. The candle-stage is 0.
The candles-burned-out is a truth state that varies. The candles-burned-out is false.
The candle-timer-active is a truth state that varies. The candle-timer-active is false.
After taking the pair of candles:
  now the candle-timer-active is true;
  continue the action.
Every turn when the pair of candles is lit and the candle-timer-active is true (this is the candle timer rule):
  increase the candle-turns by 1;
  if the candle-stage is 0 and the candle-turns is at least 40:
    now the candle-stage is 1;
    if the player can see the pair of candles:
      say "The candles grow shorter.[line break]";
  if the candle-stage is 1 and the candle-turns is at least 60:
    now the candle-stage is 2;
    if the player can see the pair of candles:
      say "The candles are becoming quite short.[line break]";
  if the candle-stage is 2 and the candle-turns is at least 70:
    now the candle-stage is 3;
    if the player can see the pair of candles:
      say "The candles won't last long now.[line break]";
  if the candle-turns is at least 75:
    now the candles-burned-out is true;
    now the pair of candles is not lit;
    now the candle-stage is 4;
    if the player can see the pair of candles:
      say "You'd better have more light than from the pair of candles.[line break]".
Chapter 6b - Match Lighting System
The match-lit is a truth state that varies. The match-lit is false.
The match-timer is a number that varies. The match-timer is 0.
Lighting-match is an action applying to nothing. Understand "light match" and "light a match" and "strike match" as lighting-match.
Instead of switching on the matchbook:
  try lighting-match.
Carry out lighting-match:
  if the player does not carry the matchbook:
    say "You don't have the matchbook." instead;
  if the match-lit is true:
    say "You already have a lit match." instead;
  if the match-count is 0:
    say "I'm afraid that you have run out of matches." instead;
  decrease the match-count by 1;
  if the player is in Drafty Room or the player is in Timber Room:
    say "This room is drafty, and the match goes out instantly." instead;
  now the match-lit is true;
  now the match-timer is 2;
  play the sound of match-sfx as sfx;
  say "One of the matches starts to burn."
Every turn when the match-lit is true (this is the match burn timer rule):
  decrease the match-timer by 1;
  if the match-timer is at most 0:
    now the match-lit is false;
    say "The match has gone out.[line break]";
    if in darkness:
      say "It's pitch black in here![line break]".
Extinguishing-match is an action applying to nothing. Understand "blow out match" and "extinguish match" as extinguishing-match.
Carry out extinguishing-match:
  if the match-lit is true:
    now the match-lit is false;
    now the match-timer is 0;
    say "The match is out.";
    if in darkness:
      say "[line break]It's pitch black in here!";
  otherwise:
    say "No match is lit."
-}