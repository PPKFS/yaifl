# yaifl

[![GitHub CI](https://github.com/PPKFS/yaifl/workflows/CI/badge.svg)](https://github.com/PPKFS/yaifl/actions)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Yaifl (**Y**et **A**nother **I**nteractive **F**iction **L**ibrary) is a Haskell DSL for writing parser-based text adventures (interactive fiction). It is heavily based off the excellent [Inform 7](https://ganelson.github.io/inform-website/), a language for writing interactive fiction in literate English.

This project is an adage to Inform 7, but without the layer of requiring the code to be writeable in literate English sentences. It also aims to be more generalised as a base for procedural generation of large worlds, rather than of hand-crafted small worlds - for instance, the Inform 7 logic for collecting items to list in a "you can also see" paragraph iterates through every item in the game!

Examples (and tests) will be taken from the Inform 7 examples.
