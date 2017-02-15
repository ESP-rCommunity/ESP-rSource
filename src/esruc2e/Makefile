architecture := $(shell uname -s)
ifeq ($(architecture),Darwin)
	SWTCH=
else
	SWTCH= -D
endif
SRCdir= `pwd`/..
PROG = c2e$(EXE)
default: $(PROG)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = prcoef.o suple.o suprf.o supwi.o sup2.o wind.o

$(PROG) : $(OBJECTS)
	$(MFC) $(FFLAGS) -o $(PROG) $(OBJECTS)

strip:
	strip $(PROG)

install:
	install $(SWTCH) $(PROG) $(ESPBinDir)/$(PROG)

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS)

distclean: clean

uninstall:
	-rm -f $(ESPBinDir)/$(PROG)
	
.PHONY: clean install default strip uninstall distclean
