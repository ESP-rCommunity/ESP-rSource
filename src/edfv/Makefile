architecture := $(shell uname -s)
ifeq ($(architecture),Darwin)
	SWTCH=
else
	SWTCH= -D
endif
SRCdir = `pwd`/..
COMdir = $(SRCdir)/common
DFSdir = $(SRCdir)/edfs
PRJdir = $(SRCdir)/eprj
CETCdir = $(SRCdir)/cetc
PROG = dfv$(EXE)
default: $(PROG)

.SUFFIXES: .o .F .f90
.F.o:
	$(MFC) $(FFLAGS) -c $<
.f90.o:
	$(MFC) $(FFLAGS) -c $<
.o :
	$(MCC) $(FFLAGS) -c $<

MODULE_OBJECTS = startup.o 

OBJECTS = cfdv.o arrow.o c2fdum.o cfdat.o cfdrw.o cfgrid.o cgd.o \
	common3dv.o e3dviews.o plot.o visgrd.o visual.o visvec.o 

MODULES = start_up.mod parse_command_line.mod 

$(PROG): $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(COMdir)/arrow.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
cfdat.F:
	rm -f cfdat.F
	ln -s $(DFSdir)/cfdat.F .
cfdrw.F:
	rm -f cfdrw.F
	ln -s $(COMdir)/cfdrw.F .
cfgrid.F:
	rm -f cfgrid.F
	ln -s $(COMdir)/cfgrid.F .
cgd.F:
	rm -f cgd.F
	ln -s $(DFSdir)/cgd.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
e3dviews.F:
	rm -f e3dviews.F
	ln -s $(COMdir)/e3dviews.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
plot.F:
	rm -f plot.F
	ln -s $(DFSdir)/plot.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
visgrd.F:
	rm -f visgrd.F
	ln -s $(DFSdir)/visgrd.F .
visual.F:
	rm -f visual.F
	ln -s $(DFSdir)/visual.F .
visvec.F:
	rm -f visvec.F
	ln -s $(DFSdir)/visvec.F .

strip:
	strip $(PROG)

install:
	install $(SWTCH) $(PROG) $(ESPBinDir)/$(PROG)

clean:
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	arrow.F c2fdum.F cfdat.F cfdrw.F cfgrid.F cgd.F e3dviews.F \
	common3dv.F plot.F startup.F visgrd.F visual.F visvec.F

distclean: clean

uninstall:
	-rm -f $(ESPBinDir)/$(PROG)
	
.PHONY: clean install default strip uninstall distclean
