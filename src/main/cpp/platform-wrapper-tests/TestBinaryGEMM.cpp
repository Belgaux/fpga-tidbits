#include <iostream>
#include <string.h>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>

using namespace std;
#include "platform.h"

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t s32;
typedef int64_t s64;

#include "TestBinaryGEMM.hpp"
void Run_TestBinaryGEMM(WrapperRegDriver* platform) 
{
  TestBinaryGEMM t(platform);
  printf("Hello world!\n");

  u64 w = 255;
  u64 a = 255;
  printf("%d\n", __builtin_popcount(a & w));
  t.set_W(w);
  t.set_A(a);
  t.set_W_R(1);
  t.set_W_C(64);
  t.set_A_R(64);
  t.set_A_C(1);
  t.set_W_depth(1);
  t.set_A_depth(1);

  t.set_start(1);
  while (t.get_done()!=1);
  printf("out=%ld\n", (s64) t.get_out());
  t.set_start(0);
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestBinaryGEMM(platform);

  deinitPlatform(platform);
  return 0;
}

