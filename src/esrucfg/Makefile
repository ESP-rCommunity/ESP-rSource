SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
PRJdir = $(SRCdir)/esruprj
TDFdir = $(SRCdir)/esrutdf
PROG = cfg$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = cfg.o arrow.o bsimsel.o c2fdum.o cfg3dv.o common3dv.o cread3dv.o ctread.o e3dviews.o \
	  edatabase.o edonecon.o egeometry.o emfnetw.o emkcfg.o enetmisc.o \
	  enetrewr.o eroper.o esru_misc.o esystem.o item.o mfcdat.o nwkrewr.o \
          plelev.o pltcfg.o sort.o startup.o tdfile.o 

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
bsimsel.F:
	rm -f bsimsel.F
	ln -s $(COMdir)/bsimsel.F .
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
e3dviews.F:
	rm -f e3dviews.F
	ln -s $(COMdir)/e3dviews.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
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
	arrow.F bsimsel.F c2fdum.F common3dv.F cread3dv.F ctread.F e3dviews.F edatabase.F egeometry.F emkcfg.F \
	emfnetw.F enetmisc.F enetrewr.F eroper.F esru_misc.F esystem.F item.F mfcdat.F nwkrewr.F \
        plelev.F pltcfg.F sort.F startup.F tdfile.F 
