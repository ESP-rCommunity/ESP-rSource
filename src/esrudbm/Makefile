SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
TDFdir = $(SRCdir)/esrutdf
PROG = dbm$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = jdbm.o bcffile.o c2fdum.o ctlexp.o ctread.o dbmedit.o dossupport.o edprof.o \
	egeometry.o emfnetw.o emkcfgg.o enved.o eroper.o esru_misc.o esystemg.o \
	gendbb.o hyged.o implegadb.o item.o mfcdat.o mlced.o nwkrewr.o pltcfg.o \
	prsitem.o sort.o startupg.o tdfile.o

$(PROG) : $(OBJECTS)
	$(MFC) $(FFLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emkcfgg.F:
	rm -f emkcfgg.F
	ln -s $(COMdir)/emkcfgg.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
item.F:
	rm -f item.F
	ln -s $(TDFdir)/item.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(TDFdir)/tdfile.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install: 
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean :
	rm -f core $(PROG) $(OBJECTS) c2fdum.F ctlexp.F ctread.F dossupport.F edatabase.F \
	egeometry.F emfnetw.F emkcfgg.F eroper.F esru_misc.F item.F mfcdat.F nwkrewr.F \
	pltcfg.F sort.F tdfile.F
