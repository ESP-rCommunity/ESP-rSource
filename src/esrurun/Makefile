SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
CTLdir = $(SRCdir)/esructl
DFSdir = $(SRCdir)/esrudfs
MFSdir = $(SRCdir)/esrumfs
PDFdir = $(SRCdir)/esrupdf
PRJdir = $(SRCdir)/esruprj
TDFdir = $(SRCdir)/esrutdf
CETCdir= $(SRCdir)/cetc
PROG = run$(EXE)
PROG2 = runesp$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = runesp.o aim2_pretimestep.o arrow.o ascii_mat.o \
	blcond.o bnlthp.o bpfcom.o bsimsel.o \
	c2fdum.o cfdat.o cfdrw.o cfg3dv.o cfgmsc.o cfgrid.o cgd.o \
	clickonbitmap.o context.o ctlexp.o ctlwrt.o ctread.o \
	dossupport.o e3dviews.o ecasctl.o \
	econstr.o econtrol.o edatabase.o edcfd.o edcon.o edcondb.o eddb.o \
	edgeo.o edipv.o editCFC.o edobs.o edonecon.o edoptic.o edpro.o edtopol.o \
	edzone.o egeometry.o egtgeom.o \
	emeta.o emfnetw.o emkcfg.o eroper.o esru_misc.o esystem.o explor.o filelist.o \
	folders.o gtopol.o hcfmk.o initalc.o insert.o item.o maintn.o mfcdat.o \
	mfelst.o mfnmsc.o mfprb1.o mfprb2.o mfrlst.o mksbem.o \
	MultiYear_climate.o net2pdf.o newnet.o nwkrewr.o \
	pltcfg.o pltnet.o prescoef.o prjdmds.o prjfmk.o prjqa.o \
	psychro.o rcdblist.o readTrnsys.o rwipv.o rwroam.o rwsbem.o sort.o \
	startup.o tdfedit.o tdfile.o tdgraph.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(PRJdir)/ascii_mat.F .
blcond.F:
	rm -f blcond.F
	ln -s $(PRJdir)/blcond.F .
bnlthp.F:
	rm -f bnlthp.F
	ln -s $(PRJdir)/bnlthp.F .
bpfcom.F:
	rm -f bpfcom.F
	ln -s $(PRJdir)/bpfcom.F .
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
cfg3dv.F:
	rm -f cfg3dv.F
	ln -s $(PRJdir)/cfg3dv.F .
cfgmsc.F:
	rm -f cfgmsc.F
	ln -s $(PDFdir)/cfgmsc.F .
cfgrid.F:
	rm -f cfgrid.F
	ln -s $(COMdir)/cfgrid.F .
cgd.F:
	rm -f cgd.F
	ln -s $(DFSdir)/cgd.F .
clickonbitmap.F:
	rm -f clickonbitmap.F
	ln -s $(PRJdir)/clickonbitmap.F .
context.F:
	rm -f context.F
	ln -s $(PRJdir)/context.F .
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
ctlwrt.F:
	rm -f ctlwrt.F
	ln -s $(PRJdir)/ctlwrt.F .
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
edcfd.F:
	rm -f edcfd.F
	ln -s $(PRJdir)/edcfd.F .
edcon.F:
	rm -f edcon.F
	ln -s $(PRJdir)/edcon.F .
edcondb.F:
	rm -f edcondb.F
	ln -s $(PRJdir)/edcondb.F .
eddb.F:
	rm -f eddb.F
	ln -s $(PRJdir)/eddb.F .
edgeo.F:
	rm -f edgeo.F
	ln -s $(PRJdir)/edgeo.F .
edipv.F:
	rm -f edipv.F
	ln -s $(PRJdir)/edipv.F .
edobs.F:
	rm -f edobs.F
	ln -s $(PRJdir)/edobs.F .
edonecon.F:
	rm -f edonecon.F
	ln -s $(PRJdir)/edonecon.F .
edoptic.F:
	rm -f edoptic.F
	ln -s $(PRJdir)/edoptic.F .
edpro.F:
	rm -f edpro.F
	ln -s $(PRJdir)/edpro.F .
edtopol.F:
	rm -f edtopol.F
	ln -s $(PRJdir)/edtopol.F .
edzone.F:
	rm -f edzone.F
	ln -s $(PRJdir)/edzone.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
egtgeom.F:
	rm -f egtgeom.F
	ln -s $(COMdir)/egtgeom.F .
emeta.F:
	rm -f emeta.F
	ln -s $(PRJdir)/emeta.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emkcfg.F:
	rm -f emkcfg.F
	ln -s $(COMdir)/emkcfg.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esystem.F:
	rm -f esystem.F
	ln -s $(COMdir)/esystem.F .
explor.F:
	rm -f explor.F
	ln -s $(PRJdir)/explor.F .
filelist.F:
	rm -f filelist.F
	ln -s $(COMdir)/filelist.F .
folders.F:
	rm -f folders.F
	ln -s $(PRJdir)/folders.F .
gtopol.F:
	rm -f gtopol.F
	ln -s $(PRJdir)/gtopol.F .
hcfmk.F:
	rm -f hcfmk.F
	ln -s $(PRJdir)/hcfmk.F .
initalc.F:
	rm -f initalc.F
	ln -s $(CTLdir)/initalc.F .
insert.F:
	rm -f insert.F
	ln -s $(PRJdir)/insert.F .
item.F:
	rm -f item.F
	ln -s $(COMdir)/item.F .
maintn.F:
	rm -f maintn.F
	ln -s $(PRJdir)/maintn.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
mfelst.F:
	rm -f mfelst.F
	ln -s $(MFSdir)/mfelst.F .
mfnmsc.F:
	rm -f mfnmsc.F
	ln -s $(PDFdir)/mfnmsc.F .
mfprb1.F:
	rm -f mfprb1.F
	ln -s $(PRJdir)/mfprb1.F .
mfprb2.F:
	rm -f mfprb2.F
	ln -s $(PRJdir)/mfprb2.F .
mfrlst.F:
	rm -f mfrlst.F
	ln -s $(MFSdir)/mfrlst.F .
mksbem.F:
	rm -f mksbem.F
	ln -s $(PRJdir)/mksbem.F .
MultiYear_climate.F:
	rm -f MultiYear_climate.F
	ln -s $(COMdir)/MultiYear_climate.F .
net2pdf.F:
	rm -f net2pdf.F
	ln -s $(PDFdir)/net2pdf.F .
newnet.F:
	rm -f newnet.F
	ln -s $(PDFdir)/newnet.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
pltnet.F:
	rm -f pltnet.F
	ln -s $(PRJdir)/pltnet.F .
prescoef.F:
	rm -f prescoef.F
	ln -s $(PRJdir)/prescoef.F .
prjdmds.F:
	rm -f prjdmds.F
	ln -s $(PRJdir)/prjdmds.F .
prjfmk.F:
	rm -f prjfmk.F
	ln -s $(PRJdir)/prjfmk.F .
prjqa.F:
	rm -f prjqa.F
	ln -s $(PRJdir)/prjqa.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
rcdblist.F:
	rm -f rcdblist.F
	ln -s $(COMdir)/rcdblist.F .
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
rwipv.F:
	rm -f rwipv.F
	ln -s $(COMdir)/rwipv.F .
rwroam.F:
	rm -f rwroam.F
	ln -s $(COMdir)/rwroam.F .
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
tdgraph.F:
	rm -f tdgraph.F
	ln -s $(PRJdir)/tdgraph.F .
tdfedit.F:
	rm -f tdfedit.F
	ln -s $(PRJdir)/tdfedit.F .
aim2_pretimestep.F:
	rm -f aim2_pretimestep.F
	ln -s $(CETCdir)/aim2_pretimestep.F .
editCFC.F:
	rm -f editCFC.F
	ln -s $(PRJdir)/editCFC.F .
strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir)/$(PROG2); fi

clean:
	rm -f core $(PROG) $(PROG2) $(PROG).exe $(PROG2).exe $(OBJECTS) \
	aim2_pretimestep.F arrow.F ascii_mat.F blcond.F bnlthp.F bpfcom.F bsimsel.F \
	c2fdum.F cfdat.F cfdrw.F cfg3dv.F cfgmsc.F cfgrid.F cgd.F clickonbitmap.F \
	context.F ctlexp.F ctlwrt.F ctread.F dossupport.F e3dviews.F ecasctl.F \
	econstr.F econtrol.F edcfd.F edcon.F edcondb.F edatabase.F eddb.F edgeo.F edipv.F \
	edobs.F edonecon.F edoptic.F edpro.F edtopol.F edzone.F egeometry.F emfnetw.F \
	egtgeom.F emkcfg.F esru_misc.F eroper.F esystem.F explor.F filelist.F folders.F \
	gtopol.F hcfmk.F initalc.F insert.F item.F maintn.F mfcdat.F mfelst.F mfprb1.F mfprb2.F \
	mfnmsc.F mfrlst.F mksbem.F MultiYear_climate.F net2pdf.F newnet.F nwkrewr.F \
	pltcfg.F pltnet.F prescoef.F prjdmds.F prjfmk.F prjqa.F psychro.F rcdblist.F \
	rwipv.F rwroam.F rwsbem.F readTrnsys.F sort.F startup.F tdfedit.F tdfile.F tdgraph.F \
	editCFC.F
	

