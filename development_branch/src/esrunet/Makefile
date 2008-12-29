SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
LIBdir = $(SRCdir)/lib
PRJdir = $(SRCdir)/esruprj
PROG = net$(EXE)

.SUFFIXES: .o .F .c
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = arrow.o common3dv.o egeometry.o edatabase.o e3dviews.o emfnetw.o eroper.o \
	esru_misc.o icndbscn.o mfcdat.o netwedit.o network.o nwkrewr.o netwkc2f.o \
	netutil.o plelev.o read3dv.o sort.o startup.o

$(PROG): $(OBJECTS) $(C_OBJ)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(CFLAGS) $(OBJECTS) $(ULIBS) 

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
e3dviews.F:
	rm -f e3dviews.F
	ln -s $(COMdir)/e3dviews.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
plelev.F:
	rm -f plelev.F
	ln -s $(COMdir)/plelev.F .
read3dv.F:
	rm -f read3dv.F
	ln -s $(PRJdir)/read3dv.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) \
	*.o arrow.F common3dv.F edatabase.F egeometry.F e3dviews.F emfnetw.F eroper.F \
	esru_misc.F plelev.F nwkrewr.F read3dv.F mfcdat.F sort.F startup.F
