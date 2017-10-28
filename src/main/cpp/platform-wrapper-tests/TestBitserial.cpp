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

  int r = 32;
  int c = 32;
  s32 w[r*c];
  s32 a[c];
  s32 e[r];
  for (int i = 0; i < r; ++i) {
    for (int j = 0; j < c; ++j) {
      int ij = i*c + j;
      w[ij] = -1;
      a[j] = 1;
      e[i] += w[ij] * a[j];
    }
  }
  printf("expected:\n");
  for (int i = 0; i < r; ++i)
    printf("%d ", e[i]);
  printf("\n");


  s32 res[r];
  for (int i = 0; i < r; ++i) {
    for (int j = 0; j < c; ++j) {
      t.set_W(w[i*c + j]);
      t.set_A(a[j]);
      t.set_bitplane(1);
      t.set_start(1);
      while (t.get_done() != 1);
      t.set_start(0);
    }
    t.set_bitplane(0);
    t.set_start(1);
    while (t.get_done() != 1);
    res[i] = t.get_out();
    t.set_start(0);
  }

  printf("result:\n");
  for (int i = 0; i < r; ++i)
    printf("%d ", res[i]);
  printf("\n");
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestBitserial(platform);

  deinitPlatform(platform);
  return 0;
}

