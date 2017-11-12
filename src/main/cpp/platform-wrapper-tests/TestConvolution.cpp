
#include <iostream>
#include <cstdint>
#include <chrono>
#include <random>
#include <unistd.h>
#include <string.h>
#include <cstdlib>
#include <cstdio>

using namespace std;
#include "platform.h"

#include "TestConvolution.hpp"

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
void Run_TestConvolution(WrapperRegDriver* platform) 
{
  TestConvolution t(platform);
  //cout << "Signature: " << hex << t.get_signature() << dec << endl;
  
  // Random generator
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);

  const int word_size_in_bits = 64;
    
  const int num_input_channels = 2, num_output_channels = 2,
    num_input_bitplanes = 4;
    
  const int image_width = 6, image_height = 6;

  const int window_size = 2, stride = 4;
  const int num_filter_bitplanes = 4;

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
  
  t.set_imageAddr((AccelDblReg)dram_image);
  t.set_filterAddr((AccelDblReg)dram_filters);
  t.set_outputAddr((AccelDblReg)dram_result);
  t.set_tempAddr((AccelDblReg)temp_buffer);
  
  t.set_imageWidth(image_width);
  t.set_imageHeight(image_height);
  t.set_imageNumBits(num_input_bitplanes);
  t.set_imageNumChannels(num_input_channels);
  
  t.set_stride(stride);
  t.set_windowSize(window_size);
  t.set_numOutputChannels(num_output_channels);
  t.set_filtersNumBits(num_filter_bitplanes);

  t.set_start(1);

  while(!t.get_finishedWithSlidingWindow());
  printf("Finished sliding window\n");
  
  while(!t.get_finished());

  t.set_start(0);
  printf("Finished entire convolution!\n");

  int64_t accel_result[expected_result_num_elements];
  platform->copyBufferAccelToHost(dram_result, accel_result, expected_result_size_in_bytes);
  printf("Copied %d bytes\n", expected_result_size_in_bytes);
#if 1
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


#if 1
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


#if 1
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

  
#if 1
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


#if 1
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

#if 1
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

  bool equal = true;
  for(int i = 0; i < expected_result_num_elements; i++){
    if(accel_result[i] != expected_result[i]){
      printf("Element number %d was different\n", i);
      equal = false;
      break;
    }
  }

  if(equal){
    printf("The results were equal!\n");
  }
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestConvolution(platform);

  deinitPlatform(platform);
  return 0;
}

