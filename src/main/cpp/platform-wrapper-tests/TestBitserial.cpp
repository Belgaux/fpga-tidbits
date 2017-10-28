#include <iostream>
#include <string.h>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <bitset>

using namespace std;
#include "platform.h"

typedef uint32_t u32;
typedef int32_t s32;

#include "TestBitserial.hpp"
void Run_TestBitserial(WrapperRegDriver* platform) 
{
  TestBitserial t(platform);

  int n = 32;
  s32 w[n];
  s32 a[n];
  for (int i = 0; i < n; ++i) {
    w[i] = -1;
    a[i] = -1;
  }

  for (int i = 0; i < n; ++i) {
    t.set_W(w[i]);
    t.set_A(a[i]);
    t.set_bitplane(1);
    t.set_start(1);
    while (t.get_done() != 1);
    t.set_start(0);
  }

  t.set_bitplane(0);
  t.set_start(1);
  while (t.get_done() != 1);

  s32 res = t.get_out();
  printf("result=%d\n", res);

  t.set_start(0);
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestBitserial(platform);

  deinitPlatform(platform);
  return 0;
}

