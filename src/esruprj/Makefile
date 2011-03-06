SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
DFSdir = $(SRCdir)/esrudfs
MFSdir = $(SRCdir)/esrumfs
PDFdir = $(SRCdir)/esrupdf
VLDdir = $(SRCdir)/esruvld
PLTdir = $(SRCdir)/esruplt
CETCdir = $(SRCdir)/cetc
INCLdir = $(SRCdir)/include
PROG = prj$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS)  $(BUILD_OPTIONS) -c $<

OBJECTS = prj.o anlytc.o aim2_module.o aim2_inputs.o arrow.o ascii_mat.o basesimp.o \
	  basesimp_inputs.o blcond.o bnlthp.o bpfcom.o bpfcontrl.o bscoeff.o bscoeff_extended.o \
	  bsimsel.o c2fdum.o cadio.o cfdat.o cfdrw.o cfg3dv.o cfgmsc.o cfgrid.o cgd.o \
	  clickonbitmap.o common3dv.o commonclm.o context.o ctlexp.o ctlwrt.o ctprob.o \
	  ctread.o DG_controller_menu.o DHW_module.o dossupport.o e2eplus.o \
	  e3dviews.o ecasctl.o econstr.o econtrol.o edatabase.o edcasctl.o \
	  edcfd.o edcfg.o edcon.o edcondb.o eddb.o edgeo.o edipv.o editCFC.o edmrt.o \
	  edobs.o edonecon.o edoptic.o edpro.o edspmtl.o edtopol.o edzone.o egeometry.o \
	  egrid.o egtgeom.o emeta.o emfnetw.o emkcfg.o enetmisc.o enetprj.o enetrewr.o \
	  eroper.o esru_misc.o esystem.o exlgrad.o explor.o filelist.o folders.o \
	  ground_temp_mains.o gtopol.o h3k_utils.o hcfmk.o hvacgui.o initalc.o insert.o item.o \
	  maintn.o mcdbscn.o mfcdat.o mfelst.o mfnmsc.o mfprb1.o mfprb2.o mfrlst.o \
	  mksbem.o MultiYear_climate.o mvalid.o \
	  net2pdf.o newnet.o nwkrewr.o plelevvc.o pltcfg.o pltnet.o prescoef.o \
	  prjdmds.o prjfmk.o prjqa.o psychro.o rcdblist.o read3dv.o readTrnsys.o \
	  rwipv.o rwroam.o rwsbem.o sendd.o sendflt.o senrwl.o sensa.o \
	  setup.o sort.o spmisc.o stndrds.o startup.o tdfedit.o tdfile.o tdgraph.o \
	  hvacinput.o \
	  PltDummy.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2K_dummy.mod aim2_inputdata.mod aim2_inputs.mod aim2_inputs_inputdata.mod	

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS) $(BUILD_OPTIONS) 

anlytc.F:
	rm -f anlytc.F
	ln -s $(VLDdir)/anlytc.F .
arrow.F:
	rm -f arrow.F
	ln -s $(COMdir)/arrow.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(COMdir)/ascii_mat.F .
basesimp.F:
	rm -f basesimp.F
	ln -s $(BLDdir)/basesimp.F .
bscoeff.F:
	rm -f bscoeff.F
	ln -s $(BLDdir)/bscoeff.F .
bscoeff_extended.F:
	rm -f bscoeff_extended.F
	ln -s $(CETCdir)/bscoeff_extended.F .
bsimsel.F:
	rm -f bsimsel.F
	ln -s $(COMdir)/bsimsel.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
cfdat.F:
	rm -f cfdat.F
	ln -s $(DFSdir)/cfdat.F .
cfdrw.F:
	rm -f cfdrw.F
	ln -s $(COMdir)/cfdrw.F .
cfgmsc.F:
	rm -f cfgmsc.F
	ln -s $(PDFdir)/cfgmsc.F .
cfgrid.F:
	rm -f cfgrid.F
	ln -s $(COMdir)/cfgrid.F .
cgd.F:
	rm -f cgd.F
	ln -s $(DFSdir)/cgd.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
commonclm.F:
	rm -f commonclm.F
	ln -s $(COMdir)/commonclm.F .
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
ctprob.F:
	rm -f ctprob.F
	ln -s $(COMdir)/ctprob.F .
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
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
egrid.F:
	rm -f egrid.F
	ln -s $(COMdir)/egrid.F .
egtgeom.F:
	rm -f egtgeom.F
	ln -s $(COMdir)/egtgeom.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emkcfg.F:
	rm -f emkcfg.F
	ln -s $(COMdir)/emkcfg.F .
enetmisc.F:
	rm -f enetmisc.F
	ln -s $(COMdir)/enetmisc.F .
enetrewr.F:
	rm -f enetrewr.F
	ln -s $(COMdir)/enetrewr.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
esystem.F:
	rm -f esystem.F
	ln -s $(COMdir)/esystem.F .
exlgrad.F:
	rm -f exlgrad.F
	ln -s $(CETCdir)/exlgrad.F .
filelist.F:
	rm -f filelist.F
	ln -s $(COMdir)/filelist.F .
h3k_utils.F:
	rm -f h3k_utils.F
	ln -s $(CETCdir)/h3k_utils.F .
initalc.F:
	rm -f initalc.F
	ln -s $(COMdir)/initalc.F .
item.F:
	rm -f item.F
	ln -s $(COMdir)/item.F .
mcdbscn.F:
	rm -f mcdbscn.F
	ln -s $(COMdir)/mcdbscn.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
mfelst.F:
	rm -f mfelst.F
	ln -s $(MFSdir)/mfelst.F .
mfnmsc.F:
	rm -f mfnmsc.F
	ln -s $(PDFdir)/mfnmsc.F .
mfrlst.F:
	rm -f mfrlst.F
	ln -s $(MFSdir)/mfrlst.F .
MultiYear_climate.F:
	rm -f MultiYear_climate.F
	ln -s $(COMdir)/MultiYear_climate.F .
mvalid.F:
	rm -f mvalid.F
	ln -s $(VLDdir)/mvalid.F .
net2pdf.F:
	rm -f net2pdf.F
	ln -s $(PDFdir)/net2pdf.F .
newnet.F:
	rm -f newnet.F
	ln -s $(PDFdir)/newnet.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
plelevvc.F:
	rm -f plelevvc.F
	ln -s $(COMdir)/plelevvc.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
rcdblist.F:
	rm -f rcdblist.F
	ln -s $(COMdir)/rcdblist.F .
rwipv.F:
	rm -f rwipv.F
	ln -s $(COMdir)/rwipv.F .
rwroam.F:
	rm -f rwroam.F
	ln -s $(COMdir)/rwroam.F .
rwsbem.F:
	rm -f rwsbem.F
	ln -s $(COMdir)/rwsbem.F .
senrwl.F:
	rm -f senrwl.F
	ln -s $(COMdir)/senrwl.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
spmisc.F:
	rm -f spmisc.F
	ln -s $(COMdir)/spmisc.F .
stndrds.F:
	rm -f stndrds.F
	ln -s $(VLDdir)/stndrds.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .
aim2_module.F:
	rm -f aim2_module.F
	ln -s $(BLDdir)/aim2_module.F .
DG_controller_menu.F:
	rm -f DG_controller_menu.F
	ln -s $(CETCdir)/DG_controller_menu.F .
DHW_module.F:
	rm -f DHW_module.F
	ln -s $(CETCdir)/DHW_module.F .
ground_temp_mains.F:
	rm -f ground_temp_mains.F
	ln -s $(BLDdir)/ground_temp_mains.F .
hvacinput.F:
	rm -f hvacinput.F
	ln -s $(CETCdir)/hvacinput.F .
PltDummy.F:
	rm -f PltDummy.F
	ln -s $(PLTdir)/PltDummy.F .

readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) $(MODULES) \
	anlytc.F aim2_module.F arrow.F ascii_mat.F basesimp.F bscoeff.F bscoeff_extended.F \
	bsimsel.F c2fdum.F cfdat.F cfdrw.F cfgmsc.F \
	cfgrid.F cgd.F common3dv.F commonclm.F ctlexp.F ctprob.F ctread.F DG_controller_menu.F \
	DHW_module.F exlgrad.F dossupport.F e3dviews.F ecasctl.F econstr.F econtrol.F \
	edatabase.F egeometry.F egrid.F egtgeom.F emfnetw.F emkcfg.F enetmisc.F enetrewr.F \
	eroper.F esru_misc.F esystem.F filelist.F ground_temp_mains.F h3k_utils.F initalc.F \
	item.F mcdbscn.F mfcdat.F mfelst.F mfnmsc.F mfrlst.F MultiYear_climate.F mvalid.F \
	net2pdf.F newnet.F nwkrewr.F plelevvc.F pltcfg.F psychro.F rcdblist.F rwipv.F rwroam.F \
	rwsbem.F senrwl.F sort.F spmisc.F stndrds.F startup.F tdfile.F  \
	readTrnsys.F PltDummy.F





