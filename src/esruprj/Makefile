SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
CTLdir = $(SRCdir)/esructl
DFSdir = $(SRCdir)/esrudfs
MFSdir = $(SRCdir)/esrumfs
PDFdir = $(SRCdir)/esrupdf
TDFdir = $(SRCdir)/esrutdf
VLDdir = $(SRCdir)/esruvld
CETCdir = $(SRCdir)/cetc
INCLdir = $(SRCdir)/include
PROG = prj$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS)  $(BUILD_OPTIONS) -c $<

OBJECTS = prj.o anlytc.o arrow.o blcond.o bnlthp.o bpfcom.o bpfcontrl.o \
	  bsimsel.o c2fdum.o cadio.o cfdat.o cfdrw.o cfgmsc.o cfgrid.o cgd.o \
	  clickonbitmap.o common3dv.o context.o ctlwrt.o ctprob.o ctread.o e2eplus.o e3dviews.o ecasctl.o econstr.o \
	  econtrol.o edatabase.o edcasctl.o edcfd.o edcfg.o edcon.o edcondb.o \
	  eddb.o edgeo.o edipv.o edmrt.o edobs.o edoptic.o edspmtl.o edtopol.o \
	  edzone.o egeometry.o egrid.o egtgeom.o emfnetw.o emkcfg.o enetmisc.o \
	  enetprj.o enetrewr.o eroper.o esru_misc.o esystem.o filelist.o \
	  folders.o gtopol.o h3k_utils.o hcfmk.o initalc.o insert.o item.o mcdbscn.o \
	  mfcdat.o mfelst.o mfnmsc.o mfprb1.o mfprb2.o mfrlst.o mvalid.o \
	  net2pdf.o newnet.o nwkrewr.o plelev.o pltcfg.o pltnet.o prescoef.o \
	  prjdmds.o prjfmk.o prjqa.o psychro.o rcdblist.o read3dv.o rwipv.o sendd.o \
	  sendflt.o senrwl.o sensa.o sort.o spmisc.o stndrds.o startup.o tdfile.o \
	  aim2_pretimestep.o DHW_module.o exlgrad.o DG_controller_menu.o \
	  MultiYear_climate.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS) $(BUILD_OPTIONS)

anlytc.F:
	rm -f anlytc.F
	ln -s $(VLDdir)/anlytc.F .
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
ctprob.F:
	rm -f ctprob.F
	ln -s $(COMdir)/ctprob.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
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
	ln -s $(CTLdir)/initalc.F .
item.F:
	rm -f item.F
	ln -s $(TDFdir)/item.F .
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
plelev.F:
	rm -f plelev.F
	ln -s $(COMdir)/plelev.F .
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
	ln -s $(TDFdir)/tdfile.F .
aim2_pretimestep.F:
	rm -f aim2_pretimestep.F
	ln -s $(CETCdir)/aim2_pretimestep.F .
DG_controller_menu.F:
	rm -f DG_controller_menu.F
	ln -s $(CETCdir)/DG_controller_menu.F .
DHW_module.F:
	rm -f DHW_module.F
	ln -s $(CETCdir)/DHW_module.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	anlytc.F bsimsel.F c2fdum.F cfdat.F cfdrw.F cfgmsc.F cfgrid.F cgd.F common3dv.F ctprob.F \
	ctread.F e3dviews.F ecasctl.F econstr.F econtrol.F edatabase.F egeometry.F \
	egrid.F egtgeom.F emfnetw.F emkcfg.F enetmisc.F enetrewr.F eroper.F \
	esru_misc.F esystem.F filelist.F h3k_utils.F initalc.F item.F mcdbscn.F mfcdat.F \
	mfelst.F mfnmsc.F mfrlst.F mvalid.F net2pdf.F newnet.F nwkrewr.F plelev.F \
	pltcfg.F psychro.F rcdblist.F rwipv.F senrwl.F setup.F sort.F spmisc.F \
	stndrds.F startup.F tdfile.F \
	aim2_pretimestep.F DHW_module.F exlgrad.F DG_controller_menu.F \
	MultiYear_climate.F $(INCLdir)/espinstalldir.h



