
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

//Module takes in image of form channel/bitplane/row/column, outputs window by window in channel/bitplane/row/column format
void Run_TestConvolution(WrapperRegDriver* platform) 
{
  TestConvolution t(platform);
  //cout << "Signature: " << hex << t.get_signature() << dec << endl;
  
  // Random generator
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);

  const int word_size_in_bits = 64;
    
  const int num_input_channels = 2, num_output_channels = 2,
    num_input_bitplanes = 3;

  std::uniform_int_distribution<uint8_t> input_distribution(0, (1 << num_input_bitplanes) - 1);
  
  const int image_width = 3, image_height = 3;

  const int window_size = 2, stride = 1;
  const int num_filter_bitplanes = 3;

  uint8_t image[image_width*image_height*num_input_channels];

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

  int8_t filters[window_size * window_size * num_input_channels * num_output_channels];
  std::uniform_int_distribution<uint8_t> filter_distribution(0, (1 << num_filter_bitplanes) - 1);
  
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

#if 0
  printf("Image: \n");
  for(int i = 0; i < num_input_channels; i++){
    printf("Channel %d\n", i);
    for(int j = 0; j < image_height; j++){
      for(int k = 0; k < image_width; k++){
	printf("%d", image[i * image_width * image_height + j * image_width + k]);
      }
      printf("\n");
    }
    printf("\n");
  }
  printf("\n");
#endif

#if 0
  printf("Packed image:\n");
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
    for(int j = 0; j < num_input_channels; j++){
      for(int k = 0; k < window_size * window_size; k++){
	printf("%d ", filters[(i * num_input_channels + j ) * window_size * window_size + k]);
      }
      printf("   ");
    }
    printf("\n");
  }
  printf("\n");
#endif


#if 1
  printf("Packed filters: \n");
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
  printf("\n");
#endif
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestConvolution(platform);

  deinitPlatform(platform);
  return 0;
}

