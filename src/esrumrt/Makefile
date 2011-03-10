SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
PRJdir = $(SRCdir)/esruprj
CETCdir = $(SRCdir)/cetc
BLDdir = $(SRCdir)/esrubld
PROG = mrt$(EXE)
PROG2 = espvwf$(EXE)


.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = espvwf.o aim2_module.o arrow.o ascii_mat.o \
	  basesimp.o basesimp_inputs.o bscoeff.o bscoeff_extended.o \
   bound.o bubble.o c2fdum.o checkrec.o common3dv.o \
	  cread3dv.o ctread.o dossupport.o e3dviews.o econstr.o edatabase.o edmrt.o egeometry.o \
	  emfnetw.o emkcfg.o enetmisc.o eroper.o esru_misc.o esystem.o filein.o \
	  filelist.o grid.o inside.o item.o matpol.o mfcdat.o mrtinput.o nwkrewr.o \
	  plelevvc.o plne.o pltcfg.o readTrnsys.o rwipv.o rwsbem.o sort.o sparea.o startup.o \
	  surinv.o survis.o tdfile.o temper.o editCFC.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2K_dummy.mod aim2_inputdata.mod	

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

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
econstr.F:
	rm -f econstr.F
	ln -s $(COMdir)/econstr.F .
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

editCFC.F:
	rm -f editCFC.F
	ln -s $(PRJdir)/editCFC.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir)/$(PROG2); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) $(MODULES) \
	aim2_module.F arrow.F ascii_mat.F basesimp.F basesimp_inputs.F bscoeff.F bscoeff_extended.F \
	c2fdum.F common3dv.F cread3dv.F ctread.F dossupport.F e3dviews.F \
	econstr.F edatabase.F edmrt.F egeometry.F emfnetw.F enetmisc.F enetrewr.F emkcfg.F \
	eroper.F esru_misc.F esystem.F filelist.F item.F mfcdat.F nwkrewr.F editCFC.F \
	plelevvc.F pltcfg.F readTrnsys.F rwipv.F rwsbem.F sort.F startup.F tdfile.F
