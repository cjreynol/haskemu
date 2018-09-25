# HaskGB

This is a learning project for me, meant to have a larger scope than any of 
my previous Haskell projects and help me learn how to put them together on a 
bigger scale.  My overall goal is to create a Nintendo Gameboy emulator 
capable of playing a number of standard ROM files, hopefully expanding this 
list over time.

Currently, my initial step is to create a 
[CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) emulator.  After some reading 
I took some advice from forums and decided to start with a slightly 
smaller-scoped project and learn some of the basics before taking on the more 
complicated GB emulator.  My plan is to figure out a good representation for 
the pieces of the emulator like registers, memory, etc and the best way to 
fit these together and operate on them.


## Building

This project has been tested with GHC 8.4.3 and cabal-install 2.4.  With the 
2.4 release of cabal the new-style commands are in place and expected to be 
used, with the old-style versions deprecated and eventually going to be 
removed.  

- To build the project:  `cabal new-build chip8`
- To run(and potentially build):  `cabal new-run chip8`
- To build the docs:  `cabal new-haddock chip8`

