#!/bin/bash
cd ..
sbt "run e QBART Tester"
cd emu-QBART
make
./app && cd .. && cd emu-QBART
make
