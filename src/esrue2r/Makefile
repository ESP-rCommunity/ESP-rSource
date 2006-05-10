SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
MRTdir = $(SRCdir)/esrumrt
PRJdir = $(SRCdir)/esruprj
PROG = e2r$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = e2r.o arrow.o c2fdum.o common3dv.o cread3dv.o e2rfile.o e2rform.o e2rvp.o \
	  e3dviews.o ecasctl.o econstr.o edatabase.o egeometry.o egtgeom.o \
	  eroper.o esru_misc.o grid.o inside.o msurf.o plelev.o radcfg.o radcmds.o \
	  radctl.o rifform.o scsys.o sort.o startup.o

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
ecasctl.F:
	rm -f ecasctl.F
	ln -s $(COMdir)/ecasctl.F .
econstr.F:
	rm -f econstr.F
	ln -s $(COMdir)/econstr.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
egtgeom.F:
	rm -f egtgeom.F
	ln -s $(COMdir)/egtgeom.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
inside.F:
	rm -f inside.F
	ln -s $(MRTdir)/inside.F .
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
	arrow.F c2fdum.F common3dv.F cread3dv.F e3dviews.F ecasctl.F econstr.F edatabase.F \
	egeometry.F egtgeom.F eroper.F esru_misc.F inside.F plelev.F scsys.F sort.F \
        startup.F 
