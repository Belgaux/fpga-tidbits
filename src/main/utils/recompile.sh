#!/bin/bash
cd ..
sbt "run e TestConvolution Tester"
cd emu-TestConvolution/
make
./app && cd .. && cd emu-TestConvolution
