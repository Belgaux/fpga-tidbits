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

#include "QBART.hpp"

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t s32;
typedef int64_t s64;

QBART* t;

void Run_FullyConnected(WrapperRegDriver* platform) 
{
  //QBART t(platform);
  
 
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);
  std::uniform_int_distribution<s64> distribution(-1, 1); 


  // loops for testing lots of matrices
  for (int rr = 1; rr < 5; ++rr) {
    for (int cc = 1; cc < 4; ++cc) {

    ////////////// GENERATING TEST MATRICES //////////

      int word_size = 64;
      
      int wr = 23*rr;
      int wc = 31*cc;
      int wd = 2;
      s64 W[wr*wc];

      int ar = wc;
      int ac = 1;
      int ad = 2;

      //printf("\nMatrix dim: W=(%d, %d) A=(%d, %d)\n", wr, wc, ar, ac);
      //printf("\nBit depths: %d, %d\n", wd, ad);

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
#endif
      
#if 0

      printf("\nPACKED W:\n");
      for (int i = 0; i < WP_len; ++i) {
        printf("%llu ", WP[i]);
      }
      printf("\nPACKED AT:\n");
      for (int i = 0; i < ATP_len; ++i)
        printf("%llu ", ATP[i]);
#endif
      
#if 0
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
      t->set_lhs_addr((AccelDblReg) dram_w);
      t->set_rhs_addr((AccelDblReg) dram_a);
      t->set_res_addr((AccelDblReg) dram_r);

      t->set_lhs_rows(wpr);
      t->set_lhs_cols(wpc);
      t->set_lhs_bits(wpd);
      t->set_lhs_issigned(lhs_issigned);

      t->set_rhs_rows(apr);
      t->set_rhs_cols(apc);
      t->set_rhs_bits(apd);
      t->set_rhs_issigned(rhs_issigned);

      t->set_fc(1);

      t->set_num_chn(num_chn);

      t->set_start(1);
      while (t->get_done()!=1);

      // FPGA result is produced transposed also
      s64 *hw_result_trans = new s64[out_len];
      platform->copyBufferAccelToHost(dram_r, hw_result_trans, r_bytes); 

      t->set_start(0);
      t->set_fc(0);

      ////////////  NEED TO DO THIS IN SOFTWARE
      // numpy.transpose(matrix7, axes=(1, 0, 2)).tolist()
      ////////////
#if 0
      printf("hardware_result:\n");
      for (int i = 0; i < out_len; ++i)
        printf("%lld ", hw_result_trans[i]);
      printf("\n");
#endif

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

uint32_t ceilNum(uint32_t num, uint32_t align){
  return (num + align - 1)/align * align;
}

void print_lsb(uint64_t i){
  for(int k = 0; k < 64; k++){
    printf("%ld", (i>>k)&1);
  }
}

void print_lsb(uint8_t i){
  for(int k = 0; k < 8; k++){
    printf("%d", (i>>k)&1);
  }
}

//Module takes in image of form channel/bitplane/row/column, outputs channel/row/column/bitplane convoluted image
void Run_Convolution(WrapperRegDriver* platform) 
{
  //cout << "Signature: " << hex << t.get_signature() << dec << endl;

  printf("\n\nStarting convolution \n");
  
  // Random generator
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);

  const int word_size_in_bits = 64;
    
  const int num_input_channels = 3, num_output_channels = 4,
    num_input_bitplanes = 6;
    
  const int image_width = 15, image_height = 19;

  const int window_size = 11, strideExponent = 2;
  const int num_filter_bitplanes = 3;

  const int stride = 1 << strideExponent;

  // Obs, lower limit may need to be changed
  std::uniform_int_distribution<int8_t> input_distribution(-(1 << (num_input_bitplanes - 1)), (1 << (num_input_bitplanes - 1)) - 1);
  std::uniform_int_distribution<int8_t> filter_distribution(-(1 << (num_filter_bitplanes - 1)), (1 << (num_filter_bitplanes - 1)) - 1);

  if((image_width - window_size) % stride != 0){
    printf("Invalid combination of numCols, windowSize and stride\n");
    exit(-1);
  }
  
  if((image_height - window_size) % stride != 0){
    printf("Invalid combination of numRows, windowSize and stride\n");
    exit(-1);
  }

  if(window_size > image_width || window_size > image_height){
    printf("Window does not fit inside image!\n");
    exit(-1);
  }

  const int image_size_in_bytes = image_width * image_height * num_input_channels;
  // Of the form channels/rows/columns/bitplanes
  int8_t image[image_size_in_bytes];

  for(int i = 0; i < num_input_channels; i++){
    for(int j = 0; j < image_height; j++){
      for(int k = 0; k < image_width; k++){
	image[i * image_width * image_height + j * image_width + k] = input_distribution(generator);
      }
    }
  }


  const int packed_image_row_size_in_bytes = ceilNum(image_width, word_size_in_bits) / 8,
    packed_image_size_per_bitplane = image_height * packed_image_row_size_in_bytes,
    packed_image_size_per_channel = packed_image_size_per_bitplane * num_input_bitplanes,
    packed_image_size_in_bytes = packed_image_size_per_channel * num_input_channels;
  
  // Of the form channels/bitplanes/rows/columns
  uint8_t packed_image[packed_image_size_in_bytes];
  memset(packed_image, 0, packed_image_size_in_bytes);
  
  for(int i = 0; i < num_input_channels; i++){
    for(int j = 0; j < num_input_bitplanes; j++){
      for(int k = 0; k < image_height; k++){
	int currByte = 0, currBit = 0;
	for(int l = 0; l < image_width; l++){
	  packed_image[packed_image_size_per_channel * i +
		       packed_image_size_per_bitplane * j +
		       packed_image_row_size_in_bytes * k +
		       currByte]
	    |= ((image[i * image_width * image_height + k * image_width + l] >> j) & 1 ) << currBit;
	  currBit++;
	  if(currBit == 8){
	    currBit = 0;
	    currByte++;
	  }
	}
      }
    }
  }

  // OBS!!! Remember that actual input filters will have to be reversed (index i -> (w*w - 1 - i))
  // for convolution and not correlation to take place

  // Of form output_channels/input_channels/wrows/wcolumns
  int8_t filters[window_size * window_size * num_input_channels * num_output_channels];
  
  for(int i = 0; i < num_output_channels; i++){
    for(int j = 0; j < num_input_channels; j++){
      for(int k = 0; k < window_size * window_size; k++){
	filters[(i * num_input_channels + j ) * window_size * window_size + k] =
	  filter_distribution(generator);
      }
    }
  }


  
  const int packed_filters_channel_size_in_bytes = ceilNum(window_size * window_size, word_size_in_bits) / 8;
  const int packed_filters_row_size_in_bytes = packed_filters_channel_size_in_bytes * num_input_channels;
  const int packed_filters_bitplane_size_in_bytes = packed_filters_row_size_in_bytes * num_output_channels;
  const int packed_filters_size_in_bytes = packed_filters_bitplane_size_in_bytes * num_filter_bitplanes;
  uint8_t packed_filters[packed_filters_size_in_bytes];
  memset(packed_filters, 0, packed_filters_size_in_bytes);
  for(int i = 0; i < num_filter_bitplanes; i++){
    for(int j = 0; j < num_output_channels; j++){
      for(int k = 0; k < num_input_channels; k++){
	int currByte = 0;
	int currBit = 0;
	for(int l = 0; l < window_size * window_size; l++){
	  packed_filters[i * packed_filters_bitplane_size_in_bytes +
			 j * packed_filters_row_size_in_bytes +
			 k * packed_filters_channel_size_in_bytes +
			 currByte] |=
	    ((filters[j * window_size * window_size * num_input_channels +
		      k * window_size * window_size +
		      l] >> i) & 1) << currBit;
	  currBit++;
	  if(currBit == 8){
	    currBit = 0;
	    currByte++;
	  }
	}
      }
    }
  }


  const int expected_result_width = (image_width - window_size)/stride + 1;
  const int expected_result_height = (image_height - window_size)/stride + 1;
  const int expected_result_num_elements = expected_result_width * expected_result_height * num_output_channels;
  const int expected_result_size_in_bytes = expected_result_num_elements * sizeof(int64_t);
  int64_t expected_result[expected_result_num_elements];
  memset(expected_result, 0, expected_result_size_in_bytes);
  
  for(int ci = 0; ci < num_input_channels; ci++){
    for(int co = 0; co < num_output_channels; co++){
      for(int i = 0; i < expected_result_height; i++){
	for(int j = 0; j < expected_result_width; j++){
	  for(int k = 0; k < window_size; k++){
	    for(int l = 0; l < window_size; l++){
	      expected_result[co * expected_result_width * expected_result_height  +
			      (i * expected_result_width + j)] +=
		filters[co * num_input_channels * window_size * window_size +
			ci * window_size * window_size +
			k * window_size + l] *
		image[ci * image_width * image_height +
		      (i * stride + k) * image_width +
		      (j * stride + l) ];
	    }
	  }
	}
      }
    }
  }

  // For the window-slided image:
  const int ws_window_size_in_bytes = ceilNum(window_size * window_size, word_size_in_bits) / 8;
  const int ws_row_size_in_bytes = ws_window_size_in_bytes * num_input_channels;
  const int ws_num_rows = expected_result_width * expected_result_height * num_input_bitplanes;
  const int ws_size_in_bytes = ws_num_rows * ws_row_size_in_bytes;
  
  void* dram_image = platform->allocAccelBuffer(packed_image_size_in_bytes);
  void* dram_filters = platform->allocAccelBuffer(packed_filters_size_in_bytes);
  void* dram_result = platform->allocAccelBuffer(expected_result_size_in_bytes);
  void* temp_buffer = platform->allocAccelBuffer(ws_size_in_bytes); // For output of sliding window
  
  platform->copyBufferHostToAccel(packed_image, dram_image, packed_image_size_in_bytes);
  platform->copyBufferHostToAccel(packed_filters, dram_filters, packed_filters_size_in_bytes);
  
  t->set_imageAddr((AccelDblReg)dram_image);
  t->set_filterAddr((AccelDblReg)dram_filters);
  t->set_outputAddr((AccelDblReg)dram_result);
  t->set_tempAddr((AccelDblReg)temp_buffer);
  
  t->set_imageWidth(image_width);
  t->set_imageHeight(image_height);
  t->set_imageNumBits(num_input_bitplanes);
  t->set_imageNumChannels(num_input_channels);
  
  t->set_strideExponent(strideExponent);
  t->set_windowSize(window_size);
  t->set_numOutputChannels(num_output_channels);
  t->set_filtersNumBits(num_filter_bitplanes);

  t->set_conv(1);
  
  t->set_start(1);

  while(!t->get_finishedWithSlidingWindow());
  printf("Finished sliding window\n");
  
  while(!t->get_done());

  t->set_start(0);
  t->set_conv(0);
  printf("Finished entire convolution!\n");

  int64_t accel_result[expected_result_num_elements];
  platform->copyBufferAccelToHost(dram_result, accel_result, expected_result_size_in_bytes);
  printf("Copied %d bytes\n", expected_result_size_in_bytes);

  int64_t transposed_accel_result[expected_result_num_elements];
  for(int c = 0; c < num_output_channels; c++){
    for(int j = 0; j < expected_result_width * expected_result_height; j++){
      transposed_accel_result[c * expected_result_width * expected_result_height
			      + j] =
	accel_result[j * num_output_channels + c];
    }
  }
  
#if 0
  printf("Image: \n");
  for(int i = 0; i < num_input_channels; i++){
    printf("Channel %d\n", i);
    for(int j = 0; j < image_height; j++){
      for(int k = 0; k < image_width; k++){
	printf("%d   ", image[i * image_width * image_height + j * image_width + k]);
      }
      printf("\n");
    }
    printf("\n");
  }
  printf("\n");
#endif

#if 0
  printf("Packed image (LSB):\n");
  for(int i = 0; i < num_input_channels; i++){
    printf("Channel %d\n", i);
    for(int j = 0; j < num_input_bitplanes; j++){
      printf("Bitplane %d\n", j);
      for(int k = 0; k < image_height; k++){
	int currByte = 0, currBit = 0;
	for(int l = 0; l < packed_image_row_size_in_bytes; l++){
	  print_lsb(packed_image[packed_image_size_per_channel * i + packed_image_size_per_bitplane * j + packed_image_row_size_in_bytes * k + l]);
	}
	printf("\n");
      }
      printf("\n");
    }
  }
  printf("\n");
#endif


#if 0
  printf("Filters: \n");
  for(int i = 0; i < num_output_channels; i++){
    printf("Output channel %d\n", i);
    for(int j = 0; j < num_input_channels; j++){
      printf("Input channel %d\n", j);
      for(int k = 0; k < window_size; k++){
	for(int l = 0; l < window_size; l++){
	  printf("%d   ", filters[(i * num_input_channels + j ) * window_size * window_size + k * window_size + l]);
	}
	printf("\n");
      }
      printf("\n");
    }
    printf("\n");
  }
#endif


#if 0
  printf("Packed filters (LSB): \n");
  for(int i = 0; i < num_filter_bitplanes; i++){
    printf("Bitplane %d:\n", i);
    for(int j = 0; j < num_output_channels; j++){
      for(int k = 0; k < num_input_channels; k++){
	int currByte = 0;
	int currBit = 0;
	for(int l = 0; l < packed_filters_channel_size_in_bytes; l++){
	  print_lsb(packed_filters[i * packed_filters_bitplane_size_in_bytes +
				   j * packed_filters_row_size_in_bytes +
				   k * packed_filters_channel_size_in_bytes +
				   l]);
	}
	printf("    ");
      }
      printf("\n");
    }
    printf("\n");
  }
#endif

  
#if 0
  uint8_t sliding_result[ws_size_in_bytes];
  platform->copyBufferAccelToHost(temp_buffer, sliding_result, ws_size_in_bytes);
  
  printf("Result from sliding window:\n");
  for(int i = 0; i < num_input_bitplanes; i++){
    printf("Bitplane %d:\n", i);
    for(int j = 0; j < expected_result_width * expected_result_height; j++){
      for(int k = 0; k < ws_row_size_in_bytes; k++){
	print_lsb(sliding_result[i * ws_row_size_in_bytes * expected_result_width * expected_result_height +
				 j * ws_row_size_in_bytes +
				 k]);
      }
      printf("\n");
    }
    printf("\n");
  }
#endif


#if 0
  printf("Expected result: \n");
  for(int c = 0; c < num_output_channels; c++){
    printf("Channel %d:\n", c);
    for(int i = 0; i < expected_result_height; i++){
      for(int j = 0; j < expected_result_width; j++){
	printf("%lld   ", expected_result[c * expected_result_height * expected_result_width +
					i * expected_result_width + j]);
      }
      printf("\n");
    }
    printf("\n");
  }
  printf("\n");
#endif

#if 0
  printf("Accel result: \n");
  for(int c = 0; c < num_output_channels; c++){
    printf("Channel %d:\n", c);
    for(int i = 0; i < expected_result_height; i++){
      for(int j = 0; j < expected_result_width; j++){
	printf("%lld   ", accel_result[c * expected_result_height * expected_result_width +
					i * expected_result_width + j]);
      }
      printf("\n");
    }
    printf("\n");
  }
  printf("\n");
#endif

#if 0
  printf("Transposed accel result: \n");
  printf("Accel result: \n");
  for(int c = 0; c < num_output_channels; c++){
    printf("Channel %d:\n", c);
    for(int i = 0; i < expected_result_height; i++){
      for(int j = 0; j < expected_result_width; j++){
	printf("%lld   ", transposed_accel_result[c * expected_result_height * expected_result_width +
					i * expected_result_width + j]);
      }
      printf("\n");
    }
    printf("\n");
  }
  printf("\n");
#endif

  bool equal = true;
  for(int i = 0; i < expected_result_num_elements; i++){
    if(transposed_accel_result[i] != expected_result[i]){
      printf("Element number %d was different\n", i);
      equal = false;
      break;
    }
  }

  if(equal){
    printf("The results were equal!\n\n");
  }

  platform->deallocAccelBuffer(dram_image);
  platform->deallocAccelBuffer(dram_filters);
  platform->deallocAccelBuffer(dram_result);
  platform->deallocAccelBuffer(temp_buffer);
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  QBART tt(platform);
  t = &tt;

  for(int i = 0; i < 4; i++){
    Run_Convolution(platform);
    Run_FullyConnected(platform);
  }
  
  deinitPlatform(platform);
  return 0;
}

