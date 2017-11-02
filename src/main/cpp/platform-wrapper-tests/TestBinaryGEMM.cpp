#include <string.h>
#include <iostream>
#include <string.h>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>
#include <chrono>
#include <random>

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
  

////////////// GENERATING TEST MATRICES //////////

/*
   naming scheme: 
   wr = W-rows
   wc = W-cols
   wd = W-depth
   etc..

   wp[x] = W-packed-x
   wpr   = W-packed-rows
   etc..
*/

  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);
  std::uniform_int_distribution<s64> distribution(-1, 1);
  int word_size = 64;
  
  /////////// W
  int wr = 8;
  int wc = 64;
  int wd = 2;
  u64 W[wr*wc];

  int ar = wc;
  int ac = 4;
  int ad = 2;
  
  //printf("W:\n");
  for (int i = 0; i < wr; ++i) {
    for (int j = 0; j < wc; ++j) {
      s64 r = distribution(generator);
      r = (r == 0 ? 1 : r);
      //printf("%ld ", r);
      W[i*wc + j] = r;
    }
    //printf("\n");
  }
  //printf("\n");
  
  //////////// PACK W
  int wpr = wr;
  int wpc = ((wc+(word_size-1))/word_size);
  int wpd = wd;
  u64 WP[wpr*wpc*wpd] = {0};
  for (int d = 0; d < wd; ++d) {
    for (int i = 0; i < wr; ++i) {
      for (int j = 0; j < wc; ++j) {
        int x = j / word_size;
        u64 t = (W[i*wc+j]);
        u64 s = (t >> d) & 1;
        WP[d*wr*wpc + i*wpc + x] |= (s << (j % word_size));
      }
    }
  }
  
  ///////// A
  u64 A[ar*ac];
  u64 AT[ac*ar];
  //printf("A:\n");
  for (int i = 0; i < ar; ++i) {
    for (int j = 0; j < ac; ++j) {
      s64 r = distribution(generator);
      r = (r==0 ? 1 : r);
      //printf("%ld ", r);
      A[i*ac + j] = r;
      AT[j*ac + i] = r; // A-transposed is what needs to be packed in memory
    }
    //printf("\n");
  }
  //printf("\nAT:\n");
  for (int i = 0; i < ac; ++i) {
    for (int j = 0; j < ar; ++j) {
      //printf("%ld ", AT[i*ac + j]);
    }
    //printf("\n");
  }
  //printf("\n");

  
  ////////// PACK AT
  int apr = ac;
  int apc = ((ar+(word_size-1))/word_size);
  int apd = ad;
  u64 AP[apr*apc*apd] = {0};
  for (int d = 0; d < ad; ++d) {
    for (int i = 0; i < ac; ++i) {
      for (int j = 0; j < ar; ++j) {
        int x = j / word_size;
        u64 t = AT[i*ac+j];
        u64 s = (t >> d) & 1;
        AP[d*ac*apc + i*apc + x] |= (s << (j % word_size));
      }
    }
  }
    
#if 1 // Matrix multiplication
  printf("\n");
  s64 result[wr*ac] = {0};
  for (int i = 0; i < wr; ++i) {
    for (int j = 0; j < ac; ++j) {
      for (int k = 0; k < wc; ++k) {
        int w = W[i*wc + k];
        //int a = A[k*ac + j];
        int a = AT[j*ac + k]; // equivalent
        result[i*ac+j] += a * w;
      }
    }
  }

  printf("Expected W*A:\n");
  for (int i = 0; i < wr; ++i) {
    for (int j = 0; j < ac; ++j) {
      printf("%ld ", result[i*ac + j]);
    }
    printf("\n");
  }
  printf("\n");
#endif


  printf("packed W sent to dram:\n");
  for (int i = 0; i < wpr*wpc*wpd; ++i)
    printf("%zu ", WP[i]);
  printf("\n");
  printf("packed AT sent to dram:\n");
  for (int i = 0; i < apr*apc*apd; ++i)
    printf("%zu ", AP[i]);
  printf("\n");




/////////////////////////////////////////


  int w_bytes = wpr * wpc * wpd * sizeof(u64);
  int a_bytes = apr * apc * apd * sizeof(u64);
  int r_bytes = wr * ac * sizeof(s64);

  // Allocate and copy matrices to DRAM
  void *dram_w = platform->allocAccelBuffer(w_bytes);
  void *dram_a = platform->allocAccelBuffer(a_bytes);
  void *dram_r = platform->allocAccelBuffer(r_bytes);
  platform->copyBufferHostToAccel(WP, dram_w, w_bytes);
  platform->copyBufferHostToAccel(AP, dram_a, a_bytes);

  // Send metadata for the packed matrices to the FPGA
  t.set_addrW((AccelDblReg) dram_w);
  t.set_addrA((AccelDblReg) dram_a);
  t.set_addrR((AccelDblReg) dram_r);
  t.set_byte_count_R(r_bytes);
  t.set_W_R(wpr);
  t.set_W_C(wpc);
  t.set_A_R(apc);
  t.set_A_C(apr);
  t.set_w_depth(wpd);
  t.set_a_depth(apd);

  t.set_start(1);
  while (t.get_done()!=1);

  s64 *res = new s64[wr*ac];
  platform->copyBufferAccelToHost(dram_r, res, r_bytes); 

  printf("\ndram:\n");
  for (int i = 0; i < wr*ac; ++i) {
      printf("%ld ", res[i]);
  }
  printf("res[31]=%ld\n", res[31]); 
  printf("\n");

  int succ = memcmp(result, res, wr*ac);
  printf("memcmp=%d\n", succ);

  delete[] res;
  platform->deallocAccelBuffer(dram_w);
  platform->deallocAccelBuffer(dram_a);
  platform->deallocAccelBuffer(dram_r);
          
  t.set_start(0);
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestBinaryGEMM(platform);

  deinitPlatform(platform);
  return 0;
}

