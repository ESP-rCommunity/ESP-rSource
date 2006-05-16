SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = clm$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = clm.o c2fdum.o cfiles.o cfrqdis.o chead.o clmgrf.o clmint.o \
          clmpsychart.o clmmmm.o clmper.o clmseq.o \
	  clmsyn.o clmxmn.o clpred.o psychro.o rcdblist.o rrite.o \
	  startup.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
rcdblist.F:
	rm -f rcdblist.F
	ln -s $(COMdir)/rcdblist.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	c2fdum.F psychro.F rcdblist.F startup.F 
