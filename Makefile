INSTALL_DIR="/home/opuk/.bin"
CONFIG_DIR="/home/opuk/.config/level_0"

all:
	ghc src/Main.hs --make -O2 -odir bin -hidir bin -isrc -o bin/level_0
	cp bin/level_0 level_0

install:
	touch $(CONFIG_DIR)/score
	cp TerminusBold.ttf $(CONFIG_DIR)
	cp bin/level_0 $(INSTALL_DIR)

clean:
	rm bin/*
