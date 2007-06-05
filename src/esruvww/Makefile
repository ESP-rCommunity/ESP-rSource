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
	strip $(PROGRAM)

install:
	cp $(PROGRAM) $(ESPbin)

clean:
	rm -f core $(PROGRAM) $(OBJECTS) wwlib.c
