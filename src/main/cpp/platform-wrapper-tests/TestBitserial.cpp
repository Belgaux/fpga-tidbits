#include <iostream>
#include <string.h>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>

using namespace std;
#include "platform.h"

typedef uint32_t u32;
typedef int32_t s32;
typedef int64_t s64;

#include "TestBitserial.hpp"
void Run_TestBitserial(WrapperRegDriver* platform) 
{
  TestBitserial t(platform);

  while (1) {
    int wr,wc,ar,ac;
    printf("Enter wr, wc, ar, ac:\n");
    cin >> wr >> wc >> ar >> ac;

    s32 w[wr*wc];
    s32 a[ar*ac];
    s64 e[wr*ac];
    int w_bytes = wr * wc * sizeof(s32);
    int a_bytes = ar * ac * sizeof(s32);
    int r_bytes = wr * ac * sizeof(s64);

    // Fill W
    for (int i = 0; i < wr; ++i) { 
      for (int j = 0; j < wc; ++j) {
        w[i*wc + j] = 1;
      }
      e[i] = 0;
    }
    // Fill A transposed
    for (int i = 0; i < ac; ++i) {
      for (int j = 0; j < ar; ++j) {
        a[i*ar + j] = 1;
      }
    }
    // Compute expected matrix product
    for (int i = 0; i < wr; ++i) {
      for (int j = 0; j < ac; ++j) {
        for (int k = 0; k < wc; ++k) {
          e[i*ac + j] += w[i*wc+k] * a[j*ac + k];
        }
      }
    }

    printf("expected:\n");
    for (int i = 0; i < wr; ++i) {
      for (int j = 0; j < ac; ++j) {
        printf("%ld ", e[i]);
      }
      printf("\n");
    } 

    void *dram_w = platform->allocAccelBuffer(w_bytes);
    void *dram_a = platform->allocAccelBuffer(a_bytes);
    void *dram_r = platform->allocAccelBuffer(r_bytes);
    platform->copyBufferHostToAccel(w, dram_w, w_bytes);
    platform->copyBufferHostToAccel(a, dram_a, a_bytes);

    t.set_addrW((AccelDblReg) dram_w);
    t.set_addrA((AccelDblReg) dram_a);
    t.set_addrR((AccelDblReg) dram_r);
    t.set_W_R(wr);
    t.set_W_C(wc);
    t.set_A_R(ar);
    t.set_A_C(ac);
    t.set_byte_count_W(w_bytes);
    t.set_byte_count_A(a_bytes);
    t.set_byte_count_R(r_bytes);
    t.set_start(1);
    while(t.get_done() != 1);

    s64 *res = new s64[wr*ac];
    platform->copyBufferAccelToHost(dram_r, res, r_bytes); 
    
    printf("result:\n");
    for (int i = 0; i < wr; ++i) {
      for (int j = 0; j < ac; ++j) {
        printf("%ld ", res[i]);
      }
      printf("\n");
    }

    int cmp = memcmp(e, res, wr);
    printf("memcmp=%d\n", cmp);

    delete[] res;
    platform->deallocAccelBuffer(dram_w);
    platform->deallocAccelBuffer(dram_a);
    platform->deallocAccelBuffer(dram_r);
    t.set_start(0);
  }
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestBitserial(platform);

  deinitPlatform(platform);
  return 0;
}

