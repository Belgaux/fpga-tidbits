
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
  
  
  uint32_t numCols, numRows, numChannels, windowSize, stride;

  numCols = 10, numRows = 10, numChannels = 8, windowSize = 5, stride = 1;
  uint8_t image[numCols * numRows * numChannels];

  for(int i = 0; i < numCols * numRows * numChannels; i++){
    image[i] = distribution(generator);
  }

  uint32_t numSlidesX = (numCols - windowSize)/stride + 1;
  uint32_t numSlidesY = (numRows - windowSize)/stride + 1;
  uint32_t resultSize = numSlidesX*numSlidesY*numChannels*windowSize*windowSize;
  uint8_t expectedResult[resultSize];

  for(int k = 0; k < numSlidesY; k++){
    for(int l = 0; l < numSlidesX; l++){
      for(int i = 0; i < windowSize; i++){
	for(int j = 0; j < windowSize; j++){
	  for(int c = 0; c < numChannels; c++){
	    expectedResult[((k * numSlidesX + l) * windowSize*windowSize + (i * windowSize + j)) * numChannels + c] = image[numChannels*((k*stride + i)*numCols + l*stride + j) + c];
	  }
	}
      }
    }
  }
  
  cout<<"Image: "<<endl;
  for(int i = 0; i < numRows; i++){
    for(int j = 0; j < numCols*numChannels; j++){
      cout<<image[numChannels*i*numCols + j]<<" ";
    }
    cout<<endl;
  }
  cout<<endl;

  cout<<"Expected output: "<<endl;
  for(int i = 0; i < numSlidesX*numSlidesY; i++){
    for(int j = 0; j < windowSize*windowSize*numChannels; j++){
      cout<<expectedResult[i*windowSize*windowSize*numChannels + j]<<" ";
    }
    cout<<endl;
  }
  cout<<endl;

  //We presume one byte per channel
  void* dramImage = platform->allocAccelBuffer(numCols*numRows*numChannels);
  void* dramResult = platform->allocAccelBuffer(resultSize);

  platform->copyBufferHostToAccel(image, dramImage, numCols*numRows*numChannels);
  
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
    for(int j = 0; j < windowSize*windowSize*numChannels; j++){
      //cout<<resultBuffer[i*windowSize*windowSize*numChannels + j]<<" ";
      printf("%c ", resultBuffer[i*windowSize*windowSize*numChannels + j]);
    }
    cout<<endl;
  }
  cout<<endl;

  bool ok = true;
  for(int i = 0; i < resultSize; i++){
    if(resultBuffer[i] != expectedResult[i]){
      ok = false;
      cout<<"The "<<i<<"'th of "<<resultSize<<" result bytes  were unequal"<<endl;
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

  Run_TestSlidingWindow(platform);

  deinitPlatform(platform);
  return 0;
}

