#include <stdio.h>
#include <stdlib.h>
#include "stresstest.h"

int main(int argc, char* argv[]) {
  const int elements = argc>1 ? argv[1] : 32
  context ctx;
  ctx.source.put(elements);
  ctx.wait();
  return CnC::CNC_Success;
}

int step_1::execute(const int & t, context & c ) const {
  
int i = t;
int data;
c.item_1.get(i, data);
c.item_2.put(i, ++data);
c.tags_2.put(i);

  return CnC::CNC_Success;
}

int step_2::execute(const int & t, context & c ) const {
  
int i = t;
int data;
c.item_2.get(i, data);
c.item_3.put(i, ++data);
c.tags_3.put(i);

  return CnC::CNC_Success;
}

int source::execute(const int & t, context & c ) const {
  
for(int i=0;i<16; ++i) {
  c.item_1.put(i,1);
  c.tag_1.put(i);
}

  return CnC::CNC_Success;
}

int sink::execute(const int & t, context & c ) const {
  
int results[16];
for(int i=0;i<16; ++i) {
  c.item_3.get(i,results[i]);
  printf("results[%d]: %d\n",i,results[i]);
}

  return CnC::CNC_Success;
}

