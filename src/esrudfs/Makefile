SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PRJdir = $(SRCdir)/esruprj
PROG = dfs$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = dfs.o arrow.o ascii_mat.o bndarybl.o bndaryl.o bndaryt.o c2fdum.o cfcalc.o cfdat.o \
	  cfconv.o cfdrw.o cfdumb.o cfdumm.o cfgrid.o cfmatl.o cfmgerc.o cftext.o \
	  cfutil.o cgd.o chkconvg.o common3dv.o conc.o e3dviews.o edatabase.o egeometry.o \
	  eroper.o esru_misc.o lisolv.o locmage.o mitzero.o plelev.o plot.o psychro.o \
	  read3dv.o sort.o startup.o visgrd.o visual.o visvec.o wvapour.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(PRJdir)/ascii_mat.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
cfdrw.F:
	rm -f cfdrw.F
	ln -s $(COMdir)/cfdrw.F .
cfgrid.F:
	rm -f cfgrid.F
	ln -s $(COMdir)/cfgrid.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
e3dviews.F:
	rm -f e3dviews.F
	ln -s $(COMdir)/e3dviews.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
plelev.F:
	rm -f plelev.F
	ln -s $(COMdir)/plelev.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
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
	arrow.F ascii_mat.F c2fdum.F cfdrw.F cfgrid.F common3dv.F e3dviews.F edatabase.F egeometry.F \
	eroper.F esru_misc.F plelev.F psychro.F read3dv.F sort.F startup.F 
