SRCdir= `pwd`/..
PROG = c2e$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = prcoef.o suple.o suprf.o supwi.o sup2.o wind.o

$(PROG) : $(OBJECTS)
	$(MFC) $(FFLAGS) -o $(PROG) $(OBJECTS)

install:
	strip $(PROG);cp $(PROG) $(ESPbin)
clean:
	rm -f core $(PROG) $(OBJECTS)
