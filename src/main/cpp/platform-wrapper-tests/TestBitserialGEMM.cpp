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

#include "TestBitserialGEMM.hpp"
void Run_TestBitserialGEMM(WrapperRegDriver* platform) 
{
  TestBitserialGEMM t(platform);
 
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);
  std::uniform_int_distribution<s64> distribution(-1, 1); 


  // loops for testing lots of matrices
  for (int rr = 1; rr < 4; ++rr) {
    for (int cc = 1; cc < 32; ++cc) {

    ////////////// GENERATING TEST MATRICES //////////

      int word_size = 64;
      
      int wr = rr;
      int wc = cc*64;
      int wd = 2;
      s64 W[wr*wc];

      int ar = wc;
      int ac = rr;
      int ad = 2;

      int out_rows = wr;
      int out_cols = ac;

      int lhs_issigned = 1;
      int rhs_issigned = 1;
      int num_chn = 1;
      
      /////////// W
      for (int i = 0; i < wr; ++i) {
        for (int j = 0; j < wc; ++j) {
          s64 r = distribution(generator);
          r = (r == 0 ? -1 : r);
          W[i*wc + j] = r;
        }
      }
      
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
      
      ///////// A/AT
      s64 A[ar*ac];
      s64 AT[ac*ar];
      for (int i = 0; i < ar; ++i) {
        for (int j = 0; j < ac; ++j) {
          s64 r = distribution(generator);
          r = (r==0 ? 1 : r);
          A[i*ac + j] = r;
          AT[j*ar + i] = r; // FPGA takes right-hand side transposed so we transpose A
        }
      }
            
      ////////// PACK AT
      int apr = ac;
      int apc = ((ar+(word_size-1))/word_size);
      int apd = ad;
      u64 ATP[apr*apc*apd] = {0};
      for (int d = 0; d < ad; ++d) {
        for (int i = 0; i < ac; ++i) {
          for (int j = 0; j < ar; ++j) {
            int x = j / word_size;
            u64 t = AT[i*ar+j];
            u64 s = (t >> d) & 1;
            ATP[d*ac*apc + i*apc + x] |= (s << (j % word_size));
          }
        }
      }
        
      // Matrix multiplication
      s64 sw_result[wr*ac] = {0};
      for (int i = 0; i < wr; ++i) {
        for (int j = 0; j < ac; ++j) {
          for (int k = 0; k < wc; ++k) {
            int w = W[i*wc + k];
            int a = A[k*ac + j];
            sw_result[i*ac+j] += a * w;
          }
        }
      }


#if 0
      // DEBUG PRINTING :D
      printf("W:\n");
      for (int i = 0; i < wr; ++i) {
        for (int j = 0; j < wc; ++j) {
          printf("%lld ", W[i*wc + j]);
        }
        printf("\n");
      }
      printf("A:\n");
      for (int i = 0; i < ar; ++i) {
        for (int j = 0; j < ac; ++j) {
          printf("%lld ", A[i*ac + j]);
        }
        printf("\n");
      }
      printf("\nPACKED W:\n");
      for (int i = 0; i < wpr * wpc * wpd; ++i)
        printf("%llu ", WP[i]);
      printf("\nPACKED AT:\n");
      for (int i = 0; i < apr * apc * apd; ++i)
        printf("%llu ", ATP[i]);
#endif
#if 1
      printf("\nSoftware result:\n");
      for (int i = 0; i < wr; ++i) {
        for (int j = 0; j < ac; ++j) {
          printf("%lld ", sw_result[i*ac+j]);
        }
        printf("\n");
      }
#endif


    /////////////////////////////////////////


      int w_bytes = wpr * wpc * wpd * sizeof(u64);
      int a_bytes = apr * apc * apd * sizeof(u64);
      int r_bytes = out_rows * out_cols * sizeof(s64);

      // Allocate and copy matrices to DRAM
      void *dram_w = platform->allocAccelBuffer(w_bytes);
      void *dram_a = platform->allocAccelBuffer(a_bytes);
      void *dram_r = platform->allocAccelBuffer(r_bytes);
      platform->copyBufferHostToAccel(WP, dram_w, w_bytes);
      platform->copyBufferHostToAccel(ATP, dram_a, a_bytes);

      // Send metadata for the packed matrices to the FPGA
      t.set_lhs_addr((AccelDblReg) dram_w);
      t.set_rhs_addr((AccelDblReg) dram_a);
      t.set_res_addr((AccelDblReg) dram_r);

      t.set_lhs_rows(wpr);
      t.set_lhs_cols(wpc);
      t.set_lhs_bits(wpd);
      t.set_lhs_issigned(lhs_issigned);

      t.set_rhs_rows(apr);
      t.set_rhs_cols(apc);
      t.set_rhs_bits(apd);
      t.set_rhs_issigned(rhs_issigned);

      t.set_num_chn(num_chn);

      t.set_start(1);
      while (t.get_done()!=1);

      // FPGA result is produced transposed also
      s64 *hw_result_trans = new s64[out_rows * out_cols];
      s64 *hw_result       = new s64[out_rows * out_cols];
      platform->copyBufferAccelToHost(dram_r, hw_result_trans, r_bytes); 

      t.set_start(0);

      // Transpose to get the correct result
      for (int i = 0; i < out_rows; ++i) {
        for (int j = 0; j < out_cols; ++j) {
          s64 r = hw_result_trans[j * out_rows + i];
          hw_result[i * out_cols + j] = r;
        }
      }
#if 1
      printf("Hardware result:\n");
      for (int i = 0; i < out_rows; ++i) {
        for (int j = 0; j < out_cols; ++j) {
          printf("%lld ", hw_result[i * out_cols + j]);
        }
        printf("\n");
      }
#endif

      int succ = memcmp(sw_result, hw_result, out_rows * out_cols * sizeof(s64));
      if (succ != 0) {
        printf("memcmp=%d\n", succ);
        printf("%d %d\n", rr, cc);
      }

      delete[] hw_result_trans;
      delete[] hw_result;
      platform->deallocAccelBuffer(dram_w);
      platform->deallocAccelBuffer(dram_a);
      platform->deallocAccelBuffer(dram_r);
              
    }
  }
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestBitserialGEMM(platform);

  deinitPlatform(platform);
  return 0;
}

