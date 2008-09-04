SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = clm$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = clm.o c2fdum.o cfiles.o cfrqdis.o chead.o clmgrf.o clmint.o \
          clmpsychart.o clmmmm.o clmper.o clmseq.o clmsyn.o \
	  clmxmn.o clpred.o ctlexp.o dossupport.o psychro.o rcdblist.o \
	  rrite.o startup.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
rcdblist.F:
	rm -f rcdblist.F
	ln -s $(COMdir)/rcdblist.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) \
	c2fdum.F ctlexp.F dossupport.F psychro.F rcdblist.F startup.F 
