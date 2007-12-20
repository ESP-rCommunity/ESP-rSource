SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
MRTdir = $(SRCdir)/esrumrt
PRJdir = $(SRCdir)/esruprj
TDFdir = $(SRCdir)/esrutdf
PROG= aco$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = aco.o acocalc.o acoesp.o arrow.o ascii_mat.o c2fdum.o common3dv.o cread3dv.o \
	ctlexp.o ctread.o dossupport.o e3dviews.o econstr.o edatabase.o \
	edcondb.o egeometry.o emfnetw.o enetmisc.o eroper.o esru_misc.o \
	esystem.o item.o mfcdat.o nwkrewr.o plelev.o pltcfg.o readaco.o \
	readBCF.o readTrnsys.o sort.o startup.o tdfile.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o  $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(PRJdir)/ascii_mat.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
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
inside.F:
	rm -f inside.F
	ln -s $(MRTdir)/inside.F .
item.F:
	rm -f item.F
	ln -s $(TDFdir)/item.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
plelev.F:
	rm -f plelev.F
	ln -s $(COMdir)/plelev.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(TDFdir)/tdfile.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi


clean:
	rm -f core $(PROG) $(OBJECTS) \
	arrow.F ascii_mat.F blcond.F c2fdum.F common3dv.F cread3dv.F ctlexp.F ctread.F dossupport.F \
	e3dviews.F ecasctl.F econstr.F edcon.F edcondb.F edatabase.F egeometry.F egtgeom.F \
	emfnetw.F enetmisc.F enetrewr.F emkcfg.F eroper.F esru_misc.F esystem.F inside.F \
	item.F mfcdat.F nwkrewr.F plelev.F pltcfg.F readTrnsys.F sort.F startup.F tdfile.F 
