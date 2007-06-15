SRCdir = `pwd`/..
PRJdir = $(SRCdir)/esruprj
COMdir = $(SRCdir)/esrucom
PROG = ecnv$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = ecnv.o ascii_mat.o blk_extr.o c2fdum.o e2tsbi3.o e2vdxf.o e2zip.o \
	  edatabase.o egeometry.o egtgeom.o eroper.o esru_misc.o low_lev.o \
	  mid_lev.o scsys.o sort.o startup.o wwxfig.o zipcnv.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(PRJdir)/ascii_mat.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
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
scsys.F:
	rm -f scsys.F
	ln -s $(COMdir)/scsys.F .
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
	rm -f core $(PROG) $(OBJECTS) \
	ascii_mat.F c2fdum.F edatabase.F egeometry.F egtgeom.F eroper.F esru_misc.F scsys.F sort.F startup.F 
