SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
CTLdir = $(SRCdir)/esructl
DFSdir = $(SRCdir)/esrudfs
MFSdir = $(SRCdir)/esrumfs
PRJdir = $(SRCdir)/esruprj
TDFdir = $(SRCdir)/esrutdf
CETCdir = $(SRCdir)/cetc
PROG = res$(EXE)
REPdir = $(SRCdir)/cetc/h3kreports

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = res.o arrow.o ascii_mat.o c2fdum.o castyp.o cfdat.o cfgrid.o cfutil.o cgd.o \
	  comfort.o convect2.o ctread.o design.o dmdrep.o dossupport.o e3dviews.o ecasctl.o \
	  econstr.o econtrol.o edatabase.o egeometry.o emfnetw.o enerbs.o \
	  enerbz.o enetmisc.o enetrdisp.o enetres.o enetrewr.o eroper.o \
	  esru_misc.o esystem.o fabcon.o fabtmp.o fanger.o frqdis.o iaq.o initalc.o \
	  item.o ipvrep.o librec.o mfcdat.o mfget.o mfoutp.o mfres.o mfutil.o \
	  mocfd.o moclim.o moget.o moplnt.o mosensa.o nwkrewr.o plot3d.o \
	  pltcfg.o psychart.o psychro.o readTrnsys.o recover.o replsim.o resdef.o \
	  resutil.o rlibget.o rwipv.o rwsbem.o setres.o sort.o spmatl.o spmisc.o startup.o \
	  stats.o table.o tdfile.o tdrecres.o temps.o tgraph.o utils.o \
	  vgraph.o visgrd.o visual.o visvec.o editCFC.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(COMdir)/ascii_mat.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
cfdat.F:
	rm -f cfdat.F
	ln -s $(DFSdir)/cfdat.F .
cfgrid.F:
	rm -f cfgrid.F
	ln -s $(COMdir)/cfgrid.F .
cfutil.F:
	rm -f cfutil.F
	ln -s $(DFSdir)/cfutil.F .
cgd.F:
	rm -f cgd.F
	ln -s $(DFSdir)/cgd.F .
convect2.F:
	rm -f convect2.F
	ln -s $(BLDdir)/convect2.F .
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
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
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
fanger.F:
	rm -f fanger.F
	ln -s $(COMdir)/fanger.F .
initalc.F:
	rm -f initalc.F
	ln -s $(CTLdir)/initalc.F .
item.F:
	rm -f item.F
	ln -s $(COMdir)/item.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
mfutil.F:
	rm -f mfutil.F
	ln -s $(MFSdir)/mfutil.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
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
spmatl.F:
	rm -f spmatl.F
	ln -s $(BLDdir)/spmatl.F .
spmisc.F:
	rm -f spmisc.F
	ln -s $(COMdir)/spmisc.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .
visgrd.F:
	rm -f visgrd.F
	ln -s $(DFSdir)/visgrd.F .
visual.F:
	rm -f visual.F
	ln -s $(DFSdir)/visual.F .
visvec.F:
	rm -f visvec.F
	ln -s $(DFSdir)/visvec.F .

editCFC.F:
	rm -f editCFC.F
	ln -s $(PRJdir)/editCFC.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) \
	arrow.F ascii_mat.F c2fdum.F cfdat.F cfgrid.F cfutil.F cgd.F convect2.F ctread.F \
	dossupport.F e3dviews.F ecasctl.F econstr.F econtrol.F edatabase.F \
	egeometry.F emfnetw.F enetmisc.F enetrewr.F eroper.F esru_misc.F \
	esystem.F fanger.F initalc.F item.F mfcdat.F mfutil.F nwkrewr.F pltcfg.F \
	psychro.F readTrnsys.F rwipv.F rwsbem.F sort.F spmatl.F spmisc.F startup.F tdfile.F \
	visgrd.F visual.F visvec.F editCFC.F
