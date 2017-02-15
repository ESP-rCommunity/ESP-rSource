SWTCH= -D
SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = mld$(EXE)
default: $(PROG)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

MODULE_OBJECTS = startup.o

OBJECTS = mld.o c2fdum.o mycores.o psychro.o  \
	  stats.o vfr.o

MODULES = start_up.mod parse_command_line.mod

$(PROG): $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS)

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
	strip $(PROG)

install: $(PROG)
	install -D $(PROG) $(ESPBinDir)/$(PROG)

clean:
	rm -f core $(PROG)  $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) c2fdum.F psychro.F startup.F

distclean: clean

uninstall:
	-rm -f $(ESPBinDir)/$(PROG)
	
.PHONY: clean install default strip uninstall distclean
