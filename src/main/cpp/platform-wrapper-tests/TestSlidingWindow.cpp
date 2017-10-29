
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

#include "TestSlidingWindow.hpp"
void Run_TestSlidingWindow(WrapperRegDriver* platform) 
{
  TestSlidingWindow t(platform);
  //cout << "Signature: " << hex << t.get_signature() << dec << endl;

  // Random 0/1 generator
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);
  std::uniform_int_distribution<uint8_t> distribution('0', '9');
  
  const uint32_t wordSize = 8; // In bytes
  const uint32_t numCols = 11, numRows = 11, numChannels = 13, windowSize = 2, stride = 3;
  const uint32_t wordsPerPixel = (numChannels + wordSize - 1)/wordSize; // Assume one byte per channel value
  const uint32_t numBytesPerPixel = wordsPerPixel * wordSize;

  printf("Num bytes per pixel: %d\n", numBytesPerPixel);
    
  uint8_t image[numCols * numRows * numBytesPerPixel];

  for(int i = 0; i < numCols * numRows; i++){
    for(int j = 0; j < numChannels; j++){
      image[i*numBytesPerPixel + j] = distribution(generator);
    }
  }

  

  const uint32_t numSlidesX = (numCols - windowSize)/stride + 1;
  const uint32_t numSlidesY = (numRows - windowSize)/stride + 1;
  const uint32_t resultSize = numSlidesX*numSlidesY*numBytesPerPixel*windowSize*windowSize;
  
  uint8_t expectedResult[resultSize];

  for(int k = 0; k < numSlidesY; k++){
    for(int l = 0; l < numSlidesX; l++){
      for(int i = 0; i < windowSize; i++){
	for(int j = 0; j < windowSize; j++){
	  for(int c = 0; c < numChannels; c++){
	    expectedResult[((k * numSlidesX + l) * windowSize*windowSize + (i * windowSize + j)) * numBytesPerPixel + c] = image[((k*stride + i)*numCols + l*stride + j)*numBytesPerPixel + c];
	  }
	}
      }
    }
  }
  
  cout<<"Image: "<<endl;
  for(int i = 0; i < numRows; i++){
    for(int j = 0; j < numCols; j++){
      for(int k = 0; k < numChannels; k++){
	cout<<image[(i*numCols + j)*numBytesPerPixel + k]<<" ";
      }
    }
    cout<<endl;
  }
  cout<<endl;

  cout<<"Expected output: "<<endl;
  for(int i = 0; i < numSlidesX*numSlidesY; i++){
    for(int j = 0; j < windowSize*windowSize; j++){
      for(int k = 0; k < numChannels; k++){
	cout<<expectedResult[(i*windowSize*windowSize + j)*numBytesPerPixel + k]<<" ";
      }
    }
    cout<<endl;
  }
  cout<<endl;

  //We presume one byte per channel
  void* dramImage = platform->allocAccelBuffer(numCols*numRows*numBytesPerPixel);
  void* dramResult = platform->allocAccelBuffer(resultSize);

  platform->copyBufferHostToAccel(image, dramImage, numCols*numRows*numBytesPerPixel);
  
  t.set_numCols(numCols);
  t.set_numRows(numRows);
  t.set_numChannels(numChannels);
  t.set_stride(stride);
  t.set_windowSize(windowSize);
  t.set_addrImage((AccelDblReg)dramImage);
  t.set_addrResult((AccelDblReg)dramResult);

  t.set_start(1);

  while(!t.get_finished());

  t.set_start(0);

  uint8_t resultBuffer[resultSize];
  platform->copyBufferAccelToHost(dramResult, resultBuffer, resultSize);

  cout<<"Actual output: "<<endl;
  for(int i = 0; i < numSlidesX*numSlidesY; i++){
    for(int j = 0; j < windowSize*windowSize; j++){
      for(int k = 0; k < numChannels; k++){
	printf("%c ", resultBuffer[(i*windowSize*windowSize + j)*numBytesPerPixel + k]);
      }
    }
    cout<<endl;
  }
  cout<<endl;

  bool ok = true;
  for(int i = 0; i < numSlidesX*numSlidesY; i++){
    for(int j = 0; j < windowSize*windowSize; j++){
      for(int k = 0; k < numChannels; k++){
	if(resultBuffer[(i * windowSize * windowSize + j)*numBytesPerPixel + k] != expectedResult[(i * windowSize * windowSize + j) * numBytesPerPixel + k]){
	  ok = false;
	  cout<<"The "<<((i * windowSize * windowSize + j)*numBytesPerPixel + k)<<"'th of "<<resultSize<<" result bytes  were unequal"<<endl;
	  break;
	}
      }
      if(!ok) break;
    }
    if(!ok) break;
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

  Run_TestSlidingWindow(platform);

  deinitPlatform(platform);
  return 0;
}

