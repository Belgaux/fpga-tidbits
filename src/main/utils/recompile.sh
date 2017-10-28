#!/bin/bash
cd ..
sbt "run e TestBitserial Tester"
cd emu-TestBitserial
make
./app && cd .. && cd emu-TestBitserial
