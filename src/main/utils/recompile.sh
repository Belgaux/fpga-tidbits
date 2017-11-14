#!/bin/bash
cd ..
sbt "run e TestUART Tester"
cd emu-TestUART/
make
./app && cd .. && cd emu-TestUART
