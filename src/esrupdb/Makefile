SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = pdb$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = startup.o pdb.o c2fdum.o pc_manip.o pcedit.o pcentr.o pcompi.o pcwrt.o \
	  pshell.o 

MODULES = start_up.mod
 
$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) $(MODULES)\
	c2fdum.F startup.F 
