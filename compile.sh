#!/bin/sh
rm bin/2hu
tcc -Wall -I/usr/include/SDL2 -lSDL2 -lSDL2_image -lSDL2_mixer -o bin/2hu src/main.c
./bin/2hu
