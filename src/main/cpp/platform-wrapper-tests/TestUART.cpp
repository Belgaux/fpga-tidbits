#include "platform.h"
#include <unistd.h>
#include "TestUART.hpp"
#include <iostream>

void Run_TestUART(WrapperRegDriver* platform){
  TestUART t(platform);

}

int main(){
  WrapperRegDriver * platform = initPlatform();
  
  Run_TestUART(platform);
  deinitPlatform(platform);
  return 0;


}
