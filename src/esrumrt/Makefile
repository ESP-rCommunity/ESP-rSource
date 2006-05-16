SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PRJdir = $(SRCdir)/esruprj
PROG = mrt$(EXE)


.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = mrt.o arrow.o bound.o bubble.o c2fdum.o checkrec.o common3dv.o cread3dv.o e3dviews.o \
	  edatabase.o edmrt.o egeometry.o emkcfg.o eroper.o esru_misc.o filein.o grid.o \
	  inside.o matpol.o mrtinput.o plelev.o plne.o scsys.o sort.o sparea.o \
	  startup.o surinv.o survis.o temper.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
cread3dv.F:
	rm -f cread3dv.F
	ln -s $(COMdir)/cread3dv.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
e3dviews.F:
	rm -f e3dviews.F
	ln -s $(COMdir)/e3dviews.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
edmrt.F:
	rm -f edmrt.F
	ln -s $(PRJdir)/edmrt.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
emkcfg.F:
	rm -f emkcfg.F
	ln -s $(COMdir)/emkcfg.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
plelev.F:
	rm -f plelev.F
	ln -s $(COMdir)/plelev.F .
scsys.F:
	rm -f scsys.F
	ln -s $(COMdir)/scsys.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	arrow.F c2fdum.F common3dv.F cread3dv.F e3dviews.F edatabase.F edmrt.F egeometry.F emkcfg.F \
	eroper.F esru_misc.F plelev.F scsys.F sort.F startup.F 
