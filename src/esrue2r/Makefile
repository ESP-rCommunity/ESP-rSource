SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
MRTdir = $(SRCdir)/esrumrt
PRJdir = $(SRCdir)/esruprj
CETCdir = $(SRCdir)/cetc
BLDdir = $(SRCdir)/esrubld
PROG = e2r$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

%.o : %.f90
	$(MCC) $(FFLAGS) -c $<

MODULE_OBJECTS = h3kmodule.o startup.o aim2_module.o

OBJECTS = e2r.o arrow.o ascii_mat.o basesimp.o basesimp_inputs.o bscoeff.o \
	  bscoeff_extended.o c2fdum.o cfc2rad.o common3dv.o commonclm.o e2r_complex_fenestration.o cread3dv.o ctlexp.o ctread.o \
	  dossupport.o e2rfile.o e2rform.o e2rvp.o e3dviews.o ecasctl.o econstr.o econtrol.o \
	  edatabase.o edcondb.o egeometry.o egtgeom.o emfnetw.o enetmisc.o \
	  eroper.o esru_misc.o esystem.o filelist.o grid.o initalc.o inside.o item.o mfcdat.o \
	  msurf.o nwkrewr.o plelevvc.o pltcfg.o radcfg.o radcmds.o \
	  radctl.o precal.o readTrnsys.o rifform.o rwipv.o rwsbem.o sort.o tdfile.o editCFC.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2K_dummy.mod aim2_inputdata.mod start_up.mod \
	parse_command_line.mod h3kmodule.mod

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
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
commonclm.F:
	rm -f common3dv.F
	ln -s $(COMdir)/commonclm.F .
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
ecasctl.F:
	rm -f ecasctl.F
	ln -s $(COMdir)/ecasctl.F .
econstr.F:
	rm -f econstr.F
	ln -s $(COMdir)/econstr.F .
econtrol.F:
	rm -f econtrol.F
	ln -s $(COMdir)/econtrol.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
edcondb.F:
	rm -f edcondb.F
	ln -s $(PRJdir)/edcondb.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
egtgeom.F:
	rm -f egtgeom.F
	ln -s $(COMdir)/egtgeom.F .
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
initalc.F:
	rm -f initalc.F
	ln -s $(COMdir)/initalc.F .
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
plelevvc.F:
	rm -f plelevvc.F
	ln -s $(COMdir)/plelevvc.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
	
precal.F:
	rm -f precal.F
	ln -s $(BLDdir)/precal.F .
	
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

editCFC.F:
	rm -f editCFC.F
	ln -s $(PRJdir)/editCFC.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	aim2_module.F arrow.F ascii_mat.F basesimp.F basesimp_inputs.F bscoeff.F bscoeff_extended.F \
	c2fdum.F common3dv.F commonclm.F cread3dv.F ctlexp.F ctread.F e3dviews.F \
	dossupport.F ecasctl.F econstr.F econtrol.F edcondb.F edatabase.F egeometry.F egtgeom.F \
	emfnetw.F enetmisc.F eroper.F esru_misc.F esystem.F filelist.F initalc.F inside.F item.F mfcdat.F \
	nwkrewr.F plelevvc.F pltcfg.F precal.F readTrnsys.F rwipv.F rwsbem.F sort.F startup.F tdfile.F \
    editCFC.F h3kmodule.f90
