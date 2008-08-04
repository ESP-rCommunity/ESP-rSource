SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PRJdir = $(SRCdir)/esruprj
PROG = tdf$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = tdf.o c2fdum.o explor.o item.o maintn.o setup.o startup.o \
	  tdfile.o tdgraph.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
explor.F:
	rm -f explor.F
	ln -s $(PRJdir)/explor.F .
item.F:
	rm -f item.F
	ln -s $(COMdir)/item.F .
maintn.F:
	rm -f maintn.F
	ln -s $(PRJdir)/maintn.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .
tdgraph.F:
	rm -f tdgraph.F
	ln -s $(PRJdir)/tdgraph.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(OBJECTS) \
	c2fdum.F explor.F item.F maintne.F startup.F tdfile.F tdgraph.F
