SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = mld$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = startup.o mld.o c2fdum.o mycores.o psychro.o  \
	  stats.o vfr.o

MODULES = start_up.mod

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG)  $(PROG).exe $(OBJECTS) $(MODULES) c2fdum.F psychro.F startup.F
