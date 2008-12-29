SRCdir= `pwd`/..
LIBdir = $(SRCdir)/lib/esru
PROGRAM= vww$(EXE)

.SUFFIXES: .o .F
.F.o:
	f77 $(FFLAGS) -c $<

OBJECTS = vww.o cget.o wwlib.o

$(PROGRAM): $(OBJECTS) 
	cc $(CFLAGS) -o $(PROGRAM) $(OBJECTS) $(ULIBS) -lm

wwlib.c:
	rm -f wwlib.c
	ln -s $(LIBdir)/wwlib.c .

strip:
	@if [ -f $(PROGRAM) ]; then strip $(PROGRAM); fi

install:
	@if [ -f $(PROGRAM) ]; then cp $(PROGRAM) $(ESPBinDir); fi

clean:
	rm -f core $(PROGRAM) $(PROGRAM).exe $(OBJECTS) wwlib.c
