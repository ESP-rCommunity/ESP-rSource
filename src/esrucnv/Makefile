SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
MFSdir = $(SRCdir)/esrumfs
PRJdir = $(SRCdir)/esruprj
CETCdir = $(SRCdir)/cetc
BLDdir = $(SRCdir)/esrubld
PROG = ecnv$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = startup.o aim2_module.o ecnv.o ascii_mat.o basesimp.o basesimp_inputs.o \
	  bscoeff.o bscoeff_extended.o c2fdum.o ctread.o e2vrml.o \
	  e2vdxf.o edatabase.o egeometry.o egtgeom.o \
	  emfnetw.o emkcfg.o enetmisc.o enetrewr.o eroper.o esru_misc.o \
	  esystem.o filelist.o item.o low_lev.o mfcdat.o mid_lev.o \
	  nwkrewr.o pltcfg.o readTrnsys.o rwipv.o rwsbem.o sort.o \
	   tdfile.o wwxfig.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2K_dummy.mod aim2_inputdata.mod start_up.mod \
	parse_command_line.mod

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

aim2_module.F:
	rm -f aim2_module.F
	ln -s $(BLDdir)/aim2_module.F .
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
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
egtgeom.F:
	rm -f egtgeom.F
	ln -s $(COMdir)/egtgeom.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emkcfg.F:
	rm -f emkcfg.F
	ln -s $(COMdir)/emkcfg.F .
enetmisc.F:
	rm -f enetmisc.F
	ln -s $(COMdir)/enetmisc.F .
enetrewr.F:
	rm -f enetrewr.F
	ln -s $(COMdir)/enetrewr.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
esystem.F:
	rm -f esystem.F
	ln -s $(COMdir)/esystem.F .
filelist.F:
	rm -f filelist.F
	ln -s $(COMdir)/filelist.F .
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
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
rwipv.F:
	rm -f rwipv.F
	ln -s $(COMdir)/rwipv.F .
rwsbem.F:
	rm -f rwsbem.F
	ln -s $(COMdir)/rwsbem.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
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
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) $(MODULES) \
	aim2_module.F ascii_mat.F basesimp.F basesimp_inputs.F bscoeff.F bscoeff_extended.F \
	c2fdum.F ctread.F edatabase.F egeometry.F egtgeom.F \
	emfnetw.F emkcfg.F enetmisc.F enetrewr.F eroper.F esru_misc.F esystem.F \
	filelist.F item.F mfcdat.F nwkrewr.F pltcfg.F readTrnsys.F rwipv.F rwsbem.F \
	sort.F startup.F tdfile.F
