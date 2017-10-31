#!/bin/bash
cd ..
sbt "run e TestSlidingWindowBitplanes Tester"
cd emu-TestSlidingWindowBitplanes/
make
./app && cd .. && cd emu-TestSlidingWindowBitplanes
