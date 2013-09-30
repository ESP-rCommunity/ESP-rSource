SRCdir= `pwd`/..
PROG = c2e$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = prcoef.o suple.o suprf.o supwi.o sup2.o wind.o

$(PROG) : $(OBJECTS)
	$(MFC) $(FFLAGS) -o $(PROG) $(OBJECTS)

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS)
