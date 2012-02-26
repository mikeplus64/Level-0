all:
	ghc -Wall src/Main.hs --make -O2 -odir bin -hidir bin -isrc -o bin/level_0
