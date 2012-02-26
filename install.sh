#!/bin/sh

make
mkdir ~/.config/level_0
touch ~/.config/level_0/map ~/.config/level_0/score

ln -s /usr/share/fonts/TTF/TerminusBold.ttf ~/.config/level_0/font.ttf || (echo "Font not found."; echo "Enter the path to the font you want to use:"; read FONT; ln -s "$FONT" ~/.config/level_0/font.ttf)
