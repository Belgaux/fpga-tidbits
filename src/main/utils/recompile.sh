#!/bin/bash
cd ..
sbt "run e TestBitserialGEMM Tester"
cd emu-TestBitserialGEMM
make
./app && cd .. && cd emu-TestBitserialGEMM
