ARCH := intel64
M_UNAME := $(shell uname -m)
ifeq ($(M_UNAME), i686)
ARCH := ia32
endif

ifeq (,$(CNC_INSTALL_DIR))
$(info Please estblish CnC environment variables before using this Makefile.)
$(info E.g. by running cncvars.sh or cncvars.csh)
$(info More information is available in 'Getting Started > Running the samples')
$(error CNC_INSTALL_DIR is not set)
endif

CPPFLAGS := -Wall -O2 #-g3 -pg 

SOURCES := mccompiled.C

TARGETS := mccompiled

HEADERS := mccompiled.h

DEST_OBJS=$(SOURCES:.C=.o)

#OPT := -O2

all:  mccompiled

mccompiled: $(DEST_OBJS)
	$(CXX) $(CPPFLAGS) -o $@ $(DEST_OBJS) -L$(CNC_INSTALL_DIR)/lib/$(ARCH) -lcnc -ltbb -ltbbmalloc 

%.o: %.C %.h
	$(CXX) $(CPPFLAGS) -c -I$(CNC_INSTALL_DIR)/include -o $@ $<

clean:
	rm -f $(TARGETS) $(DEST_OBJS)
