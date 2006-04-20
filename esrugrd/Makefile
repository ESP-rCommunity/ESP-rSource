SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
ISHdir = $(SRCdir)/esruish
PRJdir = $(SRCdir)/esruprj
PROG = grd$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = grd.o arrow.o bgrd1d.o bgrd1m.o bgrd3d.o bgrdst.o c2fdum.o \
	  common3dv.o cread3dv.o e3dviews.o econstr.o edatabase.o edcondb.o \
	  egeometry.o egrid.o emkcfg.o emoist.o esgrid.o eroper.o esru_misc.o \
	  g3dcmp.o g3dcnc.o g3dmsc.o g3dmsh.o grd_misc.o grnd3d.o \
          plelev.o scsys.o sort.o startup.o trnsf.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
cread3dv.F:
	rm -f cread3dv.F
	ln -s $(COMdir)/cread3dv.F .
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
egrid.F:
	rm -f egrid.F
	ln -s $(COMdir)/egrid.F .
emkcfg.F:
	rm -f emkcfg.F
	ln -s $(COMdir)/emkcfg.F .
emoist.F:
	rm -f emoist.F
	ln -s $(COMdir)/emoist.F .
esgrid.F:
	rm -f esgrid.F
	ln -s $(COMdir)/esgrid.F .
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
trnsf.F:
	rm -f trnsf.F
	ln -s $(ISHdir)/trnsf.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	arrow.F c2fdum.F common3dv.F cread3dv.F e3dviews.F econstr.F edatabase.F edcondb.F \
	egeometry.F egrid.F emkcfg.F emoist.F esgrid.F eroper.F esru_misc.F plelev.F \
	scsys.F sort.F startup.F trnsf.F 
