# HaskEmu

This is a learning project for me, meant to have an overall larger scope than 
my previous projects as I work through creating different emulators, and 
hopefully pull in/understand a number of the more common Haskell libraries 
that I have not previously used.

My general goal is to create a few emulators of older gaming systems, both to 
learn a little bit more about how that hardware worked and techniques used for 
efficient, and hopefully accurate, emulation.

Currently, my initial step is to create a 
[CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) emulator.  After some reading 
it seemed like a good first choice with the right balance of complexity 
compared to the NES or Gameboy.  


## Building and Running

- To build the project:  `cabal build chip8`
- To run:  `cabal run chip8`
- To build the docs:  `cabal haddock chip8`

