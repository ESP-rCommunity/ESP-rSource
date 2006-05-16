SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
CTLdir = $(SRCdir)/esructl
DFSdir = $(SRCdir)/esrudfs
MFSdir = $(SRCdir)/esrumfs
PFSdir = $(SRCdir)/esrupfs
PLTdir = $(SRCdir)/esruplt
TDFdir = $(SRCdir)/esrutdf
CETCdir = $(SRCdir)/cetc
H3Kdir = $(SRCdir)/cetc/h3kreports
SHCdir = $(SRCdir)/shocc
LIBdir = $(SRCdir)/lib
PROG = bld$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = bld.o adaptconf2.o adjb.o bcfunc.o bctlf.o blibsv.o bmatsu.o \
	  bmatsv.o basesimp.o bscoeff.o bscoeff_extended.o btherm.o c2fdum.o casual.o \
	  chtmst.o climi.o cntmnt.o convect1.o convect2.o ctread.o dayfac.o dfsdummy.o drylink.o \
	  ecasctl.o econstr.o econtrol.o edatabase.o egeometry.o egrid.o \
	  emfnetw.o emoist.o enetbound.o enetmath.o enetmisc.o enetpost.o \
	  enetrewr.o enetsetu.o enetsolv.o eroper.o esgrid.o esru_misc.o \
	  esystem.o g3dfac.o g3dfnc.o g3dmtx.o gcfunc.o gsdmtx.o h3k_utils.o inital.o \
	  initalc.o input.o item.o matsv.o mfcdat.o mfcntl.o mfdumb.o mfmach.o \
	  mfmode.o mfsbld.o mfsbps.o mfutil.o moistr.o montrb.o mstmsc.o \
	  mzoutp.o nwkrewr.o pltcfg.o precal.o psychro.o reslib.o rnor.o senrwl.o \
	  sensit.o sentim.o simcon.o solar.o sort.o spmatl.o spmisc.o startup.o \
	  subsys.o tdfile.o tdrecbps.o tscon.o utilf.o CETC_bld_dummy.o \
	  shocc_dummy.o enet_state_variable_manipulate.o TReports_dummy.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS) 

adaptconf2.F:
	rm -f adaptconf2.F
	ln -s $(DFSdir)/adaptconf2.F .
bscoeff_extended.F:
	rm -f bscoeff_extended.F
	ln -s $(CETCdir)/bscoeff_extended.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
cntmnt.F:
	rm -f cntmnt.F
	ln -s $(MFSdir)/cntmnt.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
dfsdummy.F:
	rm -f dfsdummy.F
	ln -s $(DFSdir)/dfsdummy.F .
ecasctl.F:
	rm -f ecasctl.F
	ln -s $(COMdir)/ecasctl.F .
econstr.F:
	rm -f econstr.F
	ln -s $(COMdir)/econstr.F .
econtrol.F:
	rm -f econtrol.F
	ln -s $(COMdir)/econtrol.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
egrid.F:
	rm -f egrid.F
	ln -s $(COMdir)/egrid.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emoist.F:
	rm -f emoist.F
	ln -s $(COMdir)/emoist.F .
enetbound.F:
	rm -f enetbound.F
	ln -s $(PFSdir)/enetbound.F .
enetmath.F:
	rm -f enetmath.F
	ln -s $(PFSdir)/enetmath.F .
enetmisc.F:
	rm -f enetmisc.F
	ln -s $(COMdir)/enetmisc.F .
enetpost.F:
	rm -f enetpost.F
	ln -s $(PFSdir)/enetpost.F .
enetrewr.F:
	rm -f enetrewr.F
	ln -s $(COMdir)/enetrewr.F .
enetsetu.F:
	rm -f enetsetu.F
	ln -s $(PFSdir)/enetsetu.F .
enetsolv.F:
	rm -f enetsolv.F
	ln -s $(PFSdir)/enetsolv.F .
enet_state_variable_manipulate.F:
	rm -f enet_state_variable_manipulate.F
	ln -s $(PFSdir)/enet_state_variable_manipulate.F
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esgrid.F:
	rm -f esgrid.F
	ln -s $(COMdir)/esgrid.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
esystem.F:
	rm -f esystem.F
	ln -s $(COMdir)/esystem.F .
gcfunc.F:
	rm -f gcfunc.F
	ln -s $(CTLdir)/gcfunc.F .
h3k_utils.F:
	rm -f h3k_utils.F
	ln -s $(CETCdir)/h3k_utils.F .
initalc.F:
	rm -f initalc.F
	ln -s $(CTLdir)/initalc.F .
item.F:
	rm -f item.F
	ln -s $(TDFdir)/item.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
mfcntl.F:
	rm -f mfcntl.F
	ln -s $(MFSdir)/mfcntl.F .
mfdumb.F:
	rm -f mfdumb.F
	ln -s $(MFSdir)/mfdumb.F .
mfmach.F:
	rm -f mfmach.F
	ln -s $(MFSdir)/mfmach.F .
mfmode.F:
	rm -f mfmode.F
	ln -s $(MFSdir)/mfmode.F .
mfsbld.F:
	rm -f mfsbld.F
	ln -s $(MFSdir)/mfsbld.F .
mfsbps.F:
	rm -f mfsbps.F
	ln -s $(MFSdir)/mfsbps.F .
mfutil.F:
	rm -f mfutil.F
	ln -s $(MFSdir)/mfutil.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
rnor.F:
	rm -f rnor.F
	ln -s $(COMdir)/rnor.F .
senrwl.F:
	rm -f senrwl.F
	ln -s $(COMdir)/senrwl.F .
shocc_dummy.c:
	rm -f shocc_dummy.c
	ln -s $(SHCdir)/shocc_dummy.c .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
spmisc.F:
	rm -f spmisc.F
	ln -s $(COMdir)/spmisc.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(TDFdir)/tdfile.F .
tdrecbps.F:
	rm -f tdrecbps.F
	ln -s $(TDFdir)/tdrecbps.F .
TReports_dummy.F:
	rm -f TReports_dummy.F
	ln -s $(H3Kdir)/TReports_dummy.F

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	adaptconf2.F bscoeff_extended.F c2fdum.F cntmnt.F ctread.F dfsdummy.F \
	ecasctl.F econstr.F econtrol.F edatabase.F egeometry.F egrid.F \
	emfnetw.F emoist.F enetbound.F enetmath.F enetmisc.F enetpost.F \
	enetrewr.F enetsetu.F enetsolv.F eroper.F esgrid.F esru_misc.F \
	esystem.F gcfunc.F h3k_utils.F initalc.F item.F mfcdat.F mfcntl.F mfdumb.F mfmach.F \
	mfmode.F mfsbld.F mfsbps.F mfutil.F nwkrewr.F pltcfg.F psychro.F rnor.F \
	senrwl.F shocc_dummy.c sort.F spmisc.F startup.F tdfile.F tdrecbps.F \
	enet_state_variable_manipulate.F TReports_dummy.F 




