SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
PRJdir = $(SRCdir)/esruprj
TDFdir = $(SRCdir)/esrutdf
PROG = ecnv$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = ecnv.o ascii_mat.o blk_extr.o c2fdum.o ctread.o e2tsbi3.o \
	  e2vdxf.o e2zip.o edatabase.o egeometry.o egtgeom.o \
	  emfnetw.o enetmisc.o enetrewr.o eroper.o esru_misc.o \
	  esystem.o item.o low_lev.o mfcdat.o mid_lev.o \
	  nwkrewr.o plelev.o pltcfg.o readTrnsys.o sort.o startup.o \
	  tdfile.o wwxfig.o zipcnv.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(PRJdir)/ascii_mat.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
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
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
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
	ascii_mat.F c2fdum.F ctread.F edatabase.F egeometry.F egtgeom.F \
	emfnetw.F enetmisc.F enetrewr.F eroper.F esru_misc.F esystem.F \
	item.F mfcdat.F nwkrewr.F plelev.F pltcfg.F readTrnsys.F sort.F \
	startup.F tdfile.F
