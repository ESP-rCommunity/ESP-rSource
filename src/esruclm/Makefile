prefix = /opt/esru
SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = clm$(EXE)
default: $(PROG)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

MODULE_OBJECTS = startup.o

OBJECTS = clm.o c2fdum.o cfiles.o cfrqdis.o chead.o clmgrf.o clmint.o \
          clmpsychart.o clmmmm.o clmper.o clmseq.o clmsyn.o \
	  clmxmn.o clpred.o commonclm.o ctlexp.o dossupport.o psychro.o rcdblist.o \
	  rrite.o

MODULES = start_up.mod parse_command_line.mod

$(PROG): $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
commonclm.F:
	rm -f commonclm.F
	ln -s $(COMdir)/commonclm.F .
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

strip: $(PROG)
	strip $(PROG)

install:
	install -D $(PROG) $(ESPBinDir)/$(PROG)

clean:
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	c2fdum.F commonclm.F ctlexp.F dossupport.F psychro.F rcdblist.F startup.F

distclean: clean

uninstall:
	-rm -f $(ESPBinDir)/$(PROG)
	
.PHONY: clean install default strip uninstall distclean
