#include <iostream>
#include <string.h>
#include <cstdint>
#include <cstdlib>
#include <cstdio>

using namespace std;
#include "platform.h"

#include "TestDotEngine.hpp"
void Run_TestDotEngine(WrapperRegDriver* platform) 
{
  TestDotEngine t(platform);

  printf("Hello world!\n");

}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestDotEngine(platform);

  deinitPlatform(platform);
  return 0;
}

