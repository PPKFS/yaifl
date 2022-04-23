# Yaifl - **Y**et **A**nother **I**nteractive **F**iction **L**ibrary

Yaifl is a library for writing interactive fiction ("text adventures") in Haskell. 
It is heavily based off [Inform7 by Graham Nelson](http://inform7.com/) with regards to the world model and standard actions, 
and uses Inform7's examples as the basis unit tests.

## But why not just use Inform?

- I doubt anyone will ever choose this over Inform7 to actually produce a work of IF.  
- The internals of Inform are not the easiest to pick apart. It does a huge amount of work under the hood and even with the
incredibly verbose debug logging, it's almost black magic. *And then* assuming you can find your way around the compiler itself,
the world model/parser/meat of the library is greatly intertwined with Inform 6.
- I want to avoid a handful of Inform's restrictions at a fundamental level (e.g. objects are considered entirely unique). To this
extent, it's less of a library for *writing* pieces of interactive fiction and more for fiddling about with *text adventure world models*.
- I'm a programmer, so whilst I have a great deal of respect for the whole English language programming thing, I'd rather see
what cool Haskell stuff I can do with it. A lot of weird things are done due to the limitations of the language itself and its targets
(i.e. the Z-Machine, or Glulx). For example, did you know that the action **to look** makes a table of **every item in the entire game**?

## Why this book?

I found I was trying to be a good samaritan and document my code, but given that the library works more in vague ideas
than function signatures, Haddock comments weren't ideal. Plus the structure of how the code needs to be to compile with
all its module dependencies is different to how I want to read my own documentation, and *also* different to how I want to
present my work if anyone finds it interesting. 

As an aside, I also I wanted to try this whole "literate programming" thing. The main reason I hadn't looked into it more before
was the large brick wall of getting a build system up as well as dealing with separating literate source from compilable/editable source.
I doubt anyone will read this book except me, but I saw [Entangled](https://entangled.github.io) presented at a talk and it seemed a cool idea I wanted to try.
