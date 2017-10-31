#!/bin/bash
cd ..
sbt "run e TestBinaryGEMM Tester"
cd emu-TestBinaryGEMM
make
./app && cd .. && cd emu-TestBinaryGEMM
