#!/bin/bash
cd ..
rm -rf emu-*
sbt "run e TestBMVM Tester"
cd emu-TestBMVM/
make
./app && cd .. && cd emu-TestBMVM
