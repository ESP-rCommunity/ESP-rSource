SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
PRJdir = $(SRCdir)/esruprj
CETCdir = $(SRCdir)/cetc
BLDdir = $(SRCdir)/esrubld
PROG = dfs$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<
%.o : %.f90
	$(MCC) $(FFLAGS) -c $<

MODULE_OBJECTS = startup.o h3kmodule.o aim2_module.o 

OBJECTS = dfs.o arrow.o ascii_mat.o bndarybl.o bndaryl.o bndaryt.o \
	c2fdum.o cfcalc.o cfdat.o cfconv.o cfdrw.o cfdumb.o cfdumm.o \
	cfgrid.o cfmatl.o cfmgerc.o cftext.o cfutil.o cgd.o \
	chkconvg.o common3dv.o conc.o  dossupport.o e3dviews.o edatabase.o \
	egeometry.o eroper.o esru_misc.o filelist.o \
	lisolv.o locmage.o mitzero.o plot.o psychro.o \
	read3dv.o sort.o visgrd.o visual.o visvec.o wvapour.o 

MODULES = aim2.mod aim2_calcdata.mod aim2_h2k_dummy.mod aim2_inputdata.mod start_up.mod \
     parse_command_line.mod h3kmodule.mod

$(PROG): $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS)

aim2_module.F:
	rm -f aim2_module.F
	ln -s $(BLDdir)/aim2_module.F .
arrow.F:
	rm -f arrow.F
	ln -s $(COMdir)/arrow.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(COMdir)/ascii_mat.F .
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
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
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
filelist.F:
	rm -f filelist.F
	ln -s $(COMdir)/filelist.F .

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
h3kmodule.f90:
	rm -f h3kmodule.f90
	ln -s $(CETCdir)/h3kmodule.f90 .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	aim2_module.F arrow.F ascii_mat.F c2fdum.F \
	cfdrw.F cfgrid.F common3dv.F dossupport.F e3dviews.F edatabase.F egeometry.F \
	eroper.F esru_misc.F filelist.F psychro.F read3dv.F sort.F startup.F \
	h3kmodule.f90
