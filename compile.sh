#!/bin/sh
g++ src/**.cpp src/**.h $(pkg-config --cflags --libs sdl2 SDL2_image SDL2_ttf) -o 2hu

