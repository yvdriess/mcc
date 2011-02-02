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

SOURCES := mccompiled.C

TARGETS := mccompiled

HEADERS := mccompiled.h

DEST_OBJS=$(SOURCES:.C=.o)

OPT := -O2

all:  mccompiled

mccompiled: $(DEST_OBJS)
	$(CXX) -Wall -g3 -ggdb -fno-inline -o $@ $(DEST_OBJS) -L$(CNC_INSTALL_DIR)/lib/$(ARCH) -lcnc -ltbb_debug -ltbbmalloc

%.o: %.C %.h
	$(CXX) -Wall -g3 -ggdb -fno-inline -c -I$(CNC_INSTALL_DIR)/include $(OPT) -o $@ $<

clean:
	rm -f $(TARGETS) $(DEST_OBJS)
