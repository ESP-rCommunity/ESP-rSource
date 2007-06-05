SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = mld$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = mld.o c2fdum.o mycores.o psychro.o startup.o \
	  stats.o vfr.o

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
	strip $(PROG)

install:
	cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) c2fdum.F psychro.F startup.F 
