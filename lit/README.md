# Yaifl - **Y**et **A**nother **I**nteractive **F**iction **L**ibrary

Yaifl is a library for writing interactive fiction ("text adventures") in Haskell. 
It is heavily based off [Inform7 by Graham Nelson](http://inform7.com/). I doubt anyone will ever choose this over Inform7 to actually produce a work of IF, but this is meant as more of an investigation as to how we can build an IF library that still has the full flexibility of the underlying language. 

The internals of Inform are not the easiest to pick apart. Even with the wonderful literate programming style (which was the inspiration for me to write this 'book'), it's a tangled web of interlocking pieces that aren't easy to find something in. Plus, I want to avoid a handful of Inform's restrictions at a fundamental level (e.g. objects are considered entirely unique). 

To this extent, it's less of a library for *writing* pieces of interactive fiction and more for fiddling about with *text adventure world models*. A lot of weird things are done due to the limitations of the language itself and its targets (i.e. the Z-Machine, or Glulx). For example, did you know that the action **to look** makes a table of **every item in the entire game**?

## Why this book?

I found I was trying to be a good samaritan and document my code, but given that the library works more in vague ideas
than function signatures, Haddock comments weren't ideal. Plus the structure of how the code needs to be to compile with
all its module dependencies is different to how I want to read my own documentation, and *also* different to how I want to
present my work if anyone finds it interesting. 

As an aside, I also I wanted to try this whole "literate programming" thing. The main reason I hadn't looked into it more before
was the large brick wall of getting a build system up as well as dealing with separating literate source from compilable/editable source.
I doubt anyone will read this book except me, but I saw [Entangled](https://entangled.github.io) presented at a talk and it seemed a cool idea I wanted to try.
