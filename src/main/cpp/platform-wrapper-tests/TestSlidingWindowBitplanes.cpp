
#include <iostream>
#include <cstdint>
#include <chrono>
#include <random>
#include <unistd.h>
#include <string.h>
#include <cstdlib>
#include <cstdio>
#include <bitset>

using namespace std;
#include "platform.h"

#include "TestSlidingWindowBitplanes.hpp"

uint32_t ceilNum(uint32_t num, uint32_t align){
  return (num + align - 1)/align * align;
}

void print_lsb(uint64_t i){
  for(int k = 0; k < 64; k++){
    printf("%ld", (i>>k)&1);
  }
  printf("\n");
}

//Module takes in image of form channel/bitplane/row/column, outputs window by window in channel/bitplane/row/column format
void Run_TestSlidingWindowBitplanes(WrapperRegDriver* platform) 
{
  TestSlidingWindowBitplanes t(platform);
  //cout << "Signature: " << hex << t.get_signature() << dec << endl;

  // Random 0/1 generator
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);
  std::uniform_int_distribution<uint8_t> distribution(0, 255);
  
  const uint32_t wordSizeInBytes = 8; // In bytes
  const uint32_t wordSizeInBits = 8*wordSizeInBytes;
  const uint32_t numCols = 27, numRows = 58, numChannels = 5, numBits = 1, windowSize = 1, stride = 1;

  if((numCols - windowSize) % stride != 0){
    printf("Invalid combination of numCols, windowSize and stride\n");
    exit(-1);
  }
  
  if((numRows - windowSize) % stride != 0){
    printf("Invalid combination of numRows, windowSize and stride\n");
    exit(-1);
  }
  
  const uint32_t numBytesPerRow = ceilNum(numCols, wordSizeInBits)/8;
  const uint32_t inputSizeInBytes = numBytesPerRow * numRows * numBits * numChannels;
  
  uint8_t image[numBytesPerRow * numRows * numBits * numChannels];

  for(int i = 0; i < numRows * numBits * numChannels; i++){
    for(int j = 0; j < numBytesPerRow; j++){
      image[i * numBytesPerRow + j] = distribution(generator);
      if(j == numCols / 8){
	int overBits = numCols - j * 8;
	image[i * numBytesPerRow + j] &= ((1<<overBits)-1);
      }else if( j > numCols /8){
	image[i * numBytesPerRow + j] = 0;
      }
    }
  }
  image[0] = 0;
  image[8] = 1;

  const uint32_t outputWindowSizeInBytes = ceilNum(windowSize*windowSize, wordSizeInBits) / 8;
  const uint32_t outputRowSizeInBytes = outputWindowSizeInBytes * numChannels;
  
  const uint32_t numWindowsX = (numCols - windowSize)/stride + 1;
  const uint32_t numWindowsY = (numRows - windowSize)/stride + 1;
  const uint32_t numOutputRows = numBits * numWindowsX * numWindowsY;
  const uint32_t outputSizeInBytes = outputRowSizeInBytes * numOutputRows;
  
  uint8_t expectedResult[outputSizeInBytes];

  for(int u = 0; u < numBits; u++){
    for(int s = 0; s < numWindowsY; s++){
      for(int t = 0; t < numWindowsX; t++){
	memset(expectedResult + (u * numWindowsY * numWindowsX + s * numWindowsX + t) * outputRowSizeInBytes,0,outputRowSizeInBytes);
	for(int j = 0; j < numChannels; j++){
	  
	  int currBit = 0;
	  int currByte = j * outputWindowSizeInBytes;
	  currByte = j * outputWindowSizeInBytes;
	  for(int l = 0; l < windowSize; l++){
	    for(int k = 0; k < windowSize; k++){
	      int imageRowByte = (t * stride + k) / 8;
	      int imageRowByteBit = t * stride + k - 8 * imageRowByte;
	      expectedResult[(u * numWindowsY * numWindowsX + s * numWindowsX + t)*outputRowSizeInBytes + currByte]
		|= ((image[((j * numBits + u) * numRows + stride * s + l) * numBytesPerRow + imageRowByte] >> imageRowByteBit ) & 1 ) << currBit;
	      currBit++;
	      if(currBit == 8){
		currBit = 0;
		currByte++;
	      }
	    }
	  }
	}
      }
    }
  }
  
  cout<<"Image (LSB): "<<endl;
  for(int i = 0; i < numRows * numBits * numChannels; i++){
    for(int j = 0; j < numBytesPerRow; j++){
      for(int k = 0; k < 8; k++){
	printf("%d", (image[i*numBytesPerRow + j] >> k ) & 1);
      }
    }
    printf("\n");
  }
  printf("\n");

  cout<<"Expected output (LSB): "<<endl;
  for(int i = 0; i < numWindowsX * numWindowsY * numBits; i++){
    for(int j = 0; j < outputRowSizeInBytes; j++){
      for(int k = 0; k < 8; k++){
	printf("%d", (expectedResult[i * outputRowSizeInBytes + j] >> k) & 1);
      }
    }
    printf("\n");
  }
  printf("\n");

  
  //We presume one byte per channel
  void* dramImage = platform->allocAccelBuffer(inputSizeInBytes);
  void* dramResult = platform->allocAccelBuffer(outputSizeInBytes);

  platform->copyBufferHostToAccel(image, dramImage, inputSizeInBytes);
  
  t.set_numCols(numCols);
  t.set_numRows(numRows);
  t.set_numChannels(numChannels);
  t.set_numBits(numBits);
  t.set_stride(stride);
  t.set_windowSize(windowSize);
  t.set_addrImage((AccelDblReg)dramImage);
  t.set_addrResult((AccelDblReg)dramResult);

  const uint64_t checkAddr = 1;
  t.set_checkAddrBRAM(checkAddr);
  
  t.set_start(1);

  while(!t.get_finished());

  t.set_start(0);

  uint64_t out = t.get_debugOutput();
  //printf("Entry no. %ld in BRAM:\n", checkAddr);
  //print_lsb(out);
  
  //return;
  uint8_t resultBuffer[outputSizeInBytes];
  platform->copyBufferAccelToHost(dramResult, resultBuffer, outputSizeInBytes);

  cout<<"Actual output: "<<endl;
  for(int i = 0; i < numOutputRows; i++){
    for(int j = 0; j < outputRowSizeInBytes; j++){
      for(int k = 0; k < 8; k++){
	printf("%d", (resultBuffer[i * outputRowSizeInBytes + j] >> k) & 1);
      }
    }
    printf("\n");
  }
  printf("\n");
  
  bool ok = true;
  for(int i = 0; i < outputSizeInBytes; i++){
    if(resultBuffer[i] != expectedResult[i]){
      printf("%d'th result bytes were unequal\n", i);
      ok = false;
      break;
    }
  }

  if(ok){
    cout<<"The results were equal!"<<endl;
  }else{
    cout<<"The results were not equal"<<endl;
  }
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestSlidingWindowBitplanes(platform);

  deinitPlatform(platform);
  return 0;
}

