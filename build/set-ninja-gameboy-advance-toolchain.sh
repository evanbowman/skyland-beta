#!/bin/bash

rm CMakeCache.txt
rm -r CMakeFiles/

cmake -GNinja -DGBA_AUTOBUILD_IMG=ON -DGBA_AUTOBUILD_CONF=ON -DCMAKE_TOOLCHAIN_FILE=$(pwd)/devkitarm.cmake .
