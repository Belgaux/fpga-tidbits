#!/bin/bash
cd ..
sbt "run e TestSlidingWindow Tester"
cd emu-TestSlidingWindow/
make
./app && cd .. && cd emu-TestSlidingWindow
