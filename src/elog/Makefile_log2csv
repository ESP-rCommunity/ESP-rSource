SRCdir = `pwd`/..
LIBdir = ../lib
COMdir = ../common
PROG = log2csv$(EXE)
MFC = lin
MCC=gcc
MCPL=g++
MFC=gfortran
CFLAGS=-I/usr/local/include -I../  -I../include -I/usr/include -DLIN -g -fno-automatic -DGCC4
FFLAGS=-I/usr/local/include -I../include -DLIN -DX11 -g -fno-automatic -DGCC4 
LD_FLAGS=-Wl,--reduce-memory-overheads
##LD_FLAGS=" "

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = log2csv.o support.o sort.o esru_fc.o esru_blk.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS)

sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
esru_fc.F:
	rm -f esru_fc.F
	ln -s $(LIBdir)/esru_fc.F .
esru_blk.F:
	rm -f esru_blk.F
	ln -s $(LIBdir)/esru_blk.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) sort.F esru_fc.F esru_blk.F
        
