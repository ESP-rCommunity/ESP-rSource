SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
CDBdir = $(SRCdir)/esrucdb
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
PRJdir = $(SRCdir)/esruprj
CETCdir = $(SRCdir)/cetc
BLDdir = $(SRCdir)/esrubld
PROG = dbm$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

%.o : %.f90
	$(MCC) $(FFLAGS) -c $<


MODULE_OBJECTS = h3kmodule.o aim2_module.o

OBJECTS = jdbm.o ascii_mat.o basesimp.o basesimp_inputs.o bscoeff.o bscoeff_extended.o \
	bcffile.o c2fdum.o ctlexp.o ctread.o dbmedit.o dossupport.o edprof.o \
	CDBPlugins.o CDBCommon.o CDBExplore.o\
	egeometry.o emfnetw.o emkcfgg.o enved.o eroper.o esru_misc.o esystemg.o \
	filelist.o gendbb.o hyged.o implegadb.o item.o \
	mfcdat.o mlced.o nwkrewr.o pltcfg.o \
	prsitem.o sort.o startupg.o tdfile.o readTrnsys.o


MODULES = aim2.mod aim2_calcdata.mod aim2_h2k_dummy.mod aim2_inputdata.mod \
          h3kmodule.mod

$(PROG) : $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(FFLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS)

aim2_module.F:
	rm -f aim2_module.F
	ln -s $(BLDdir)/aim2_module.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(COMdir)/ascii_mat.F .
basesimp.F:
	rm -f basesimp.F
	ln -s $(BLDdir)/basesimp.F .
basesimp_inputs.F:
	rm -f basesimp_inputs.F
	ln -s $(PRJdir)/basesimp_inputs.F .
bscoeff.F:
	rm -f bscoeff.F
	ln -s $(BLDdir)/bscoeff.F .
bscoeff_extended.F:
	rm -f bscoeff_extended.F
	ln -s $(CETCdir)/bscoeff_extended.F .
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
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emkcfgg.F:
	rm -f emkcfgg.F
	ln -s $(COMdir)/emkcfgg.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
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
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
h3kmodule.f90:
	rm -f h3kmodule.f90
	ln -s $(CETCdir)/h3kmodule.f90 .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean :
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	aim2_module.F ascii_mat.F basesimp.F basesimp_inputs.F bscoeff.F bscoeff_extended.F \
	CDBPlugins.F CDBCommon.F CDBExplore.F \
	c2fdum.F ctlexp.F ctread.F dossupport.F edatabase.F \
	egeometry.F emfnetw.F emkcfgg.F eroper.F esru_misc.F filelist.F item.F mfcdat.F nwkrewr.F \
	pltcfg.F sort.F tdfile.F readTrnsys.F h3kmodule.f90
