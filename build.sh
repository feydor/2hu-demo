#!/bin/sh
set -xe

if [ ! -d build ]
then
  mkdir build
fi

cmake -B build
cd build
make
cd ..
./build/2hu_demo