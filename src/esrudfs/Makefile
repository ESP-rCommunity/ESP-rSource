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

OBJECTS = dfs.o arrow.o ascii_mat.o basesimp.o basesimp_inputs.o \
     bndarybl.o bndaryl.o bndaryt.o bscoeff.o bscoeff_extended.o c2fdum.o cfcalc.o cfdat.o \
	  cfconv.o cfdrw.o cfdumb.o cfdumm.o cfgrid.o cfmatl.o cfmgerc.o cftext.o \
	  cfutil.o cgd.o chkconvg.o common3dv.o conc.o e3dviews.o econstr.o edatabase.o egeometry.o \
	  eroper.o esru_misc.o filelist.o \
	  lisolv.o locmage.o mitzero.o plot.o psychro.o \
	  read3dv.o sort.o visgrd.o visual.o visvec.o wvapour.o editCFC.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2K_dummy.mod aim2_inputdata.mod start_up.mod \
     h3kmodule.mod

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
basesimp.F:
	rm -f basesimp.F
	ln -s $(BLDdir)/basesimp.F .
basesimp_inputs.F:
	rm -f basesimp_inputs.F
	ln -s $(PRJdir)/basesimp_inputs.F .
bscoeff.F:
	rm -f bscoeff.F
	ln -s $(BLDdir)/bscoeff.F .
bscoeff_extended.F:
	rm -f bscoeff_extended.F
	ln -s $(CETCdir)/bscoeff_extended.F .
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
econstr.F:
	rm -f econstr.F
	ln -s $(COMdir)/econstr.F .
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

editCFC.F:
	rm -f editCFC.F
	ln -s $(PRJdir)/editCFC.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	aim2_module.F arrow.F ascii_mat.F \
	basesimp.F basesimp_inputs.F bscoeff.F bscoeff_extended.F \
	c2fdum.F cfdrw.F cfgrid.F common3dv.F e3dviews.F econstr.F edatabase.F editCFC.F egeometry.F \
	eroper.F esru_misc.F filelist.F psychro.F read3dv.F sort.F startup.F \
	h3kmodule.f90
