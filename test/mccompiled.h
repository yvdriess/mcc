#include <stdio.h>
#include "mccompiled.h"

int main(int argc, char* argv[]) {
  context ctx(42,43);
  ctx.wait();
  return CnC::CNC_Success;
}

int GEN::execute(const int & t, context & c ) const {
  return CnC::CNC_Success;
}

int KRON::execute(const int & t, context & c ) const {
  return CnC::CNC_Success;
}

int CZ::execute(const int & t, context & c ) const {
  return CnC::CNC_Success;
}

