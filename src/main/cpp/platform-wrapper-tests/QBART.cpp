#include <string.h>
#include <iostream>
#include <string.h>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>
#include <chrono>
#include <random>
#include <time.h>

using namespace std;
#include "platform.h"

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t s32;
typedef int64_t s64;

#include "QBART.hpp"
void Run_QBART(WrapperRegDriver* platform) 
{
  QBART t(platform);
 
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);
  std::uniform_int_distribution<s64> distribution(-1, 1); 


  // loops for testing lots of matrices
  for (int rr = 1; rr < 2; ++rr) {
    for (int cc = 1; cc < 2; ++cc) {

    ////////////// GENERATING TEST MATRICES //////////

      int word_size = 64;
      
      int wr = 4;
      int wc = 64;
      int wd = 2;
      s64 W[wr*wc];

      int ar = wc;
      int ac = 1;
      int ad = 2;

      printf("\nMatrix dim: W=(%d, %d) A=(%d, %d)\n", wr, wc, ar, ac);
      printf("\nBit depths: %d, %d\n", wd, ad);

      int out_rows = wr;
      int out_cols = ac;

      int lhs_issigned = 1;
      int rhs_issigned = 1;
      int num_chn = 1;
      int out_len = num_chn * out_rows * out_cols;
      
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
      int WP_len = wpr * wpc * wpd * num_chn;
      u64 WP[WP_len] = {0};
      for (int i = 0; i < WP_len; ++i)
        WP[i] = 0;
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
      int ATP_len = apr * apc * apd * num_chn;
      u64 ATP[ATP_len] = {0};
      for (int i = 0; i < ATP_len; ++i)
        ATP[i] = 0;
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
      clock_t b = clock();
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
      clock_t e = clock();
      double software_elapsed = double(e-b) / CLOCKS_PER_SEC;
      cout << "software elapsed: "<< software_elapsed << endl;


#if 1
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
      for (int i = 0; i < WP_len; ++i) {
        printf("%llu ", WP[i]);
      }
      printf("\nPACKED AT:\n");
      for (int i = 0; i < ATP_len; ++i)
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


      int w_bytes = WP_len * sizeof(u64);
      int a_bytes = ATP_len * sizeof(u64);
      int r_bytes = out_len * sizeof(s64);

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

      t.set_fc(1);

      t.set_num_chn(num_chn);

      clock_t begin = clock();
      t.set_start(1);
      while (t.get_done()!=1);
      clock_t end = clock();
      double hardware_elapsed = double(end-begin) / CLOCKS_PER_SEC;
      cout << "elapsed: " << hardware_elapsed << endl;
      cout << "hardware is " << software_elapsed / hardware_elapsed << " times faster." << endl;

      // FPGA result is produced transposed also
      s64 *hw_result_trans = new s64[out_len];
      platform->copyBufferAccelToHost(dram_r, hw_result_trans, r_bytes); 

      t.set_start(0);

      ////////////  NEED TO DO THIS IN SOFTWARE
      // numpy.transpose(matrix7, axes=(1, 0, 2)).tolist()
      ////////////
      for (int i = 0; i < out_len; ++i)
        printf("%lld ", hw_result_trans[i]);
      printf("\n");

      int succ = memcmp(sw_result, hw_result_trans, out_len * sizeof(s64));
      if (succ != 0) {
        printf("memcmp=%d\n", succ);
        printf("%d %d\n", rr, cc);
      }

      delete[] hw_result_trans;
      platform->deallocAccelBuffer(dram_w);
      platform->deallocAccelBuffer(dram_a);
      platform->deallocAccelBuffer(dram_r);
              
    }
  }
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_QBART(platform);

  deinitPlatform(platform);
  return 0;
}

