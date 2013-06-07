SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
CDBdir = $(SRCdir)/esrucdb
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
MRTdir = $(SRCdir)/esrumrt
PRJdir = $(SRCdir)/esruprj
CETCdir = $(SRCdir)/cetc
BLDdir = $(SRCdir)/esrubld
PROG= aco$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

%.o : %.f90
	$(MCC) $(FFLAGS) -c $<

MODULE_OBJECTS = h3kmodule.o startup.o aim2_module.o

OBJECTS = aco.o acocalc.o acoesp.o arrow.o ascii_mat.o azalt.o\
	basesimp.o basesimp_inputs.o bscoeff.o bscoeff_extended.o \
 	c2fdum.o common3dv.o CDBPlugins.o CDBCommon.o CDBExplore.o \
	cread3dv.o ctlexp.o ctread.o dossupport.o e3dviews.o econstr.o edatabase.o \
	edcondb.o egeometry.o emfnetw.o enetmisc.o eroper.o esru_misc.o \
	esystem.o filelist.o item.o mfcdat.o nwkrewr.o pltcfg.o readaco.o \
	readBCF.o readTrnsys.o rwipv.o rwsbem.o sort.o tdfile.o qrun.o \
    editCFC.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2k_dummy.mod aim2_inputdata.mod start_up.mod \
	parse_command_line.mod h3kmodule.mod

$(PROG): $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o  $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS) $(F_to_C_flags)

aim2_module.F:
	rm -f aim2_module.F
	ln -s $(BLDdir)/aim2_module.F .
arrow.F:
	rm -f arrow.F
	ln -s $(COMdir)/arrow.F .
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
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
CDBPlugins.F:
	rm -f CDBPlugins.F
	ln -s $(CDBdir)/CDBPlugins.F .
CDBCommon.F:
	rm -f CDBCommon.F
	ln -s $(CDBdir)/CDBCommon.F .
CDBExplore.F:
	rm -f CDBExplore.F
	ln -s $(CDBdir)/CDBExplore.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
cread3dv.F:
	rm -f cread3dv.F
	ln -s $(COMdir)/cread3dv.F .
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
e3dviews.F:
	rm -f e3dviews.F
	ln -s $(COMdir)/e3dviews.F .
econstr.F:
	rm -f econstr.F
	ln -s $(COMdir)/econstr.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
edcondb.F:
	rm -f edcondb.F
	ln -s $(PRJdir)/edcondb.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
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
inside.F:
	rm -f inside.F
	ln -s $(MRTdir)/inside.F .
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
rwsbem.F:
	rm -f rwsbem.F
	ln -s $(COMdir)/rwsbem.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
rwipv.F:
	rm -f rwipv.F
	ln -s $(COMdir)/rwipv.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .
azalt.F:
	rm -f azalt.F
	ln -s $(COMdir)/azalt.F .
qrun.F:
	rm -f qrun.F
	ln -s $(COMdir)/qrun.F .
h3kmodule.f90:
	rm -f h3kmodule.f90
	ln -s $(CETCdir)/h3kmodule.f90 .

editCFC.F:
	rm -f editCFC.F
	ln -s $(PRJdir)/editCFC.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi


clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) $(MODULE_OBJECTS) $(MODULES) \
	aim2_module.F arrow.F ascii_mat.F blcond.F basesimp.F basesimp_inputs.F \
	bscoeff.F bscoeff_extended.F c2fdum.F CDBPlugins.F CDBCommon.F CDBExplore.F common3dv.F cread3dv.F ctlexp.F ctread.F dossupport.F \
	e3dviews.F ecasctl.F econstr.F edcon.F edcondb.F edatabase.F egeometry.F egtgeom.F \
	emfnetw.F enetmisc.F enetrewr.F emkcfg.F eroper.F esru_misc.F esystem.F filelist.F \
	inside.F item.F mfcdat.F nwkrewr.F pltcfg.F readTrnsys.F rwipv.F rwsbem.F \
	sort.F startup.F tdfile.F editCFC.F h3kmodule.f90 qrun.F azalt.F
