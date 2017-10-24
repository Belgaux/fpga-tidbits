#!/bin/bash
cd ..
sbt "run e TestBitplane Tester"
cd emu-TestBitplane
make
./app && cd .. && cd emu-TestBitplane
