# Level 0: A Snake clone written in Haskell, using SDL
## Features
* it works
* it's fast
* readable code (it's readable to me!)
* map loading
* scoreboard

## Prerequisites
* GHC (tested with 7.0.3 and 7.4.1)
* SDL from Hackage
* SDL-ttf from Hackage
* a font (by default tries to get /usr/share/fonts/TTF/TerminusBold.ttf)

## Installation / usage
Level 0 is now on Hackage, so you can install it with `cabal install Level0`.

`$ make`

`$ bin/level_0 [ms between frames [path to map file]]`

eg

`$ bin/level_0 16 map`

I don't know if it's buildable on Windows.

A map is a plain text file, the first 32 characters on the first 32 lines are read, and when there is an 'x', you will have a wall that kills your snake when hit.
