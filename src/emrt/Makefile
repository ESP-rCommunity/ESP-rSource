architecture := $(shell uname -s)
ifeq ($(architecture),Darwin)
	SWTCH=
else
	SWTCH= -D
endif
SRCdir = `pwd`/..
BLDdir = $(SRCdir)/ebld
CDBdir = $(SRCdir)/ecdb
COMdir = $(SRCdir)/common
MFSdir = $(SRCdir)/emfs
PRJdir = $(SRCdir)/eprj
CETCdir = $(SRCdir)/cetc
PROG = mrt$(EXE)
PROG2 = espvwf$(EXE)
default: $(PROG)

.SUFFIXES: .o .F .f90
.F.o:
	$(MFC) $(FFLAGS) -c $<
.f90.o:
	$(MFC) $(FFLAGS) -c $<
.o :
	$(MCC) $(FFLAGS) -c $<

MODULE_OBJECTS = h3kmodule.o startup.o aim2_module.o CFC_Module.o

OBJECTS = espvwf.o arrow.o ascii_mat.o bound.o bubble.o c2fdum.o checkrec.o \
	  common3dv.o CDBPlugins.o CDBCommon.o CDBExplore.o cread3dv.o \
	  ctread.o dossupport.o e3dviews.o edatabase.o edmrt.o egeometry.o \
	  emfnetw.o emkcfg.o enetmisc.o eroper.o esru_misc.o esystem.o filein.o \
	  filelist.o grid.o inside.o item.o matpol.o mfcdat.o mrtinput.o nwkrewr.o \
	  plelevvc.o plne.o pltcfg.o readTrnsys.o rwipv.o rwsbem.o sort.o sparea.o \
	  surinv.o survis.o tdfile.o temper.o \
	  FMIcom.o agtcom.o grmcom.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2k_dummy.mod aim2_inputdata.mod start_up.mod \
	parse_command_line.mod h3kmodule.mod CFC_Module.mod

$(PROG): $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS)

aim2_module.F:
	rm -f aim2_module.F
	ln -s $(BLDdir)/aim2_module.F .
arrow.F:
	rm -f arrow.F
	ln -s $(COMdir)/arrow.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(COMdir)/ascii_mat.F .
CDBPlugins.F:
	rm -f CDBPlugins.F
	ln -s $(CDBdir)/CDBPlugins.F .
CDBCommon.F:
	rm -f CDBCommon.F
	ln -s $(CDBdir)/CDBCommon.F .
CDBExplore.F:
	rm -f CDBExplore.F
	ln -s $(CDBdir)/CDBExplore.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
CFC_Module.f90:
	rm -f CFC_Module.f90
	ln -s $(BLDdir)/CFC_Module.f90 .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
cread3dv.F:
	rm -f cread3dv.F
	ln -s $(COMdir)/cread3dv.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
e3dviews.F:
	rm -f e3dviews.F
	ln -s $(COMdir)/e3dviews.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
edmrt.F:
	rm -f edmrt.F
	ln -s $(PRJdir)/edmrt.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emkcfg.F:
	rm -f emkcfg.F
	ln -s $(COMdir)/emkcfg.F .
enetmisc.F:
	rm -f enetmisc.F
	ln -s $(COMdir)/enetmisc.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
esystem.F:
	rm -f esystem.F
	ln -s $(COMdir)/esystem.F .
filelist.F:
	rm -f filelist.F
	ln -s $(COMdir)/filelist.F .
item.F:
	rm -f item.F
	ln -s $(COMdir)/item.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
plelevvc.F:
	rm -f plelevvc.F
	ln -s $(COMdir)/plelevvc.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
rwipv.F:
	rm -f rwipv.F
	ln -s $(COMdir)/rwipv.F .
rwsbem.F:
	rm -f rwsbem.F
	ln -s $(COMdir)/rwsbem.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .
h3kmodule.f90:
	rm -f h3kmodule.f90
	ln -s $(CETCdir)/h3kmodule.f90 .
FMIcom.F:
	rm -f FMIcom.F
	ln -s $(COMdir)/FMIcom.F .
agtcom.F:
	rm -f agtcom.F
	ln -s $(COMdir)/agtcom.F .
grmcom.F:
	rm -f grmcom.F
	ln -s $(COMdir)/grmcom.F .
matpol.F:
	rm -f matpol.F
	ln -s $(COMdir)/matpol.F .
inside.F:
	rm -f inside.F
	ln -s $(COMdir)/inside.F .

strip:
	strip $(PROG)

install:
	install $(SWTCH) $(PROG) $(ESPBinDir)/$(PROG)

clean:
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	aim2_module.F arrow.F ascii_mat.F CDBPlugins.F CDBCommon.F CDBExplore.F \
	c2fdum.F common3dv.F cread3dv.F ctread.F dossupport.F e3dviews.F \
	edatabase.F edmrt.F egeometry.F emfnetw.F enetmisc.F enetrewr.F emkcfg.F \
	eroper.F esru_misc.F esystem.F filelist.F inside.F item.F matpol.F mfcdat.F \
	nwkrewr.F plelevvc.F pltcfg.F readTrnsys.F rwipv.F rwsbem.F sort.F startup.F \
	tdfile.F h3kmodule.f90 FMIcom.F agtcom.F grmcom.F CFC_Module.f90 cfc_module.mod

distclean: clean

uninstall:
	-rm -f $(ESPBinDir)/$(PROG)
	
.PHONY: clean install default strip uninstall distclean
