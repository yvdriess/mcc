ARCH := intel64
LANG = C
M_UNAME := $(shell uname -m)
ifeq ($(M_UNAME), i686)
ARCH := ia32
endif

ifeq (,$(CNCROOT))
$(info Please estblish CnC environment variables before using this Makefile.)
$(info E.g. by running cncvars.sh or cncvars.csh)
$(info More information is available in 'Getting Started > Running the samples')
$(error CNCROOT is not set)
endif

CPPFLAGS := -O2 #-Wall -O2 #-g3 -pg 

SOURCES := mccompiled.C

TARGETS := mccompiled

HEADERS := mccompiled.h

DEST_OBJS=$(SOURCES:.C=.o)

#OPT := -O2

all:  mccompiled

mccompiled: $(DEST_OBJS)
	$(CXX) $(CPPFLAGS) -o $@ $(DEST_OBJS) -L$(CNCROOT)/lib/$(ARCH) -lrt -lcnc -ltbb -ltbbmalloc 

%.o: %.C %.h
	$(CXX) $(CPPFLAGS) -c -I$(TBBROOT)/include -I$(CNCROOT)/include -o $@ $<

clean:
	rm -f $(TARGETS) $(DEST_OBJS)

tarball: $(TARGETS)
	mkdir -p mcc_specimen/
	cp Makefile mccompiled.C mccompiled.h mcc_specimen/
	tar cvfz mcc_specimen.tgz mcc_specimen/
