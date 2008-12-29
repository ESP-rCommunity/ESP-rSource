SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
CTLdir = $(SRCdir)/esructl
MFSdir = $(SRCdir)/esrumfs
PRJdir = $(SRCdir)/esruprj
TDFdir = $(SRCdir)/esrutdf
CETCdir= $(SRCdir)/cetc
PROG = b2e$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = b2e.o c2fdum.o clispline.o ctlwrt.o ctread.o daproc.o econtrol.o egeometry.o \
	emfnetw.o emkcfg.o eroper.o esru_misc.o esystem.o initalc.o item.o mfcdat.o \
	nwkrewr.o pltcfg.o readTrnsys.o rwipv.o rwsbem.o simcomis.o sort.o startup.o tdfile.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
ctlwrt.F:
	rm -f ctlwrt.F
	ln -s $(PRJdir)/ctlwrt.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
econtrol.F:
	rm -f econtrol.F
	ln -s $(COMdir)/econtrol.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
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
initalc.F:
	rm -f initalc.F
	ln -s $(CTLdir)/initalc.F .
item.F:
	rm -f item.F
	ln -s $(COMdir)/item.F .
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
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
rwipv.F:
	rm -f rwipv.F
	ln -s $(COMdir)/rwipv.F .
rwsbem.F:
	rm -f rwsbem.F
	ln -s $(COMdir)/rwsbem.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) \
	c2fdum.F ctlwrt.F ctread.F econtrol.F egeometry.F emfnetw.F emkcfg.F esru_misc.F \
	eroper.F esystem.F initalc.F item.F mfcdat.F nwkrewr.F pltcfg.F sort.F startup.F tdfile.F \
	readTrnsys.F rwipv.F rwsbem.F 

