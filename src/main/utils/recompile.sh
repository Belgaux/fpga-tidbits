#!/bin/bash
cd ..
sbt "run e TestDotEngine Tester"
cd emu-TestDotEngine
make
./app && cd .. && cd emu-TestDotEngine
