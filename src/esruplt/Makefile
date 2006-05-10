SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
CTLdir = $(SRCdir)/esructl
MFSdir = $(SRCdir)/esrumfs
TDFdir = $(SRCdir)/esrutdf
CETCdir = $(SRCdir)/cetc
PROG = plt$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = plt.o adjp.o c2fdum.o chpsys.o climi.o cntmnt.o cntrlf.o dfsdummy.o \
	  econtrol.o emfnetw.o h3k_utils.o inital.o initalc.o initalp.o input.o \
	  item.o mfcdat.o mfcntl.o mfdump.o mfmach.o mfmode.o mfsbps.o mfsplt.o \
	  mfutil.o monitor.o mzmixt.o pcloop.o pcomp1.o pcomp2.o pcomp3.o \
	  pcomp4.o pcomps.o pcomps1.o plibsv.o pltcfg.o pmatrx.o pmatsv.o \
	  pplib.o ppsub.o precal.o psychro.o reslib.o simcon.o startup.o \
	  tdfile.o tdrecbps.o trstyp.o tsconp.o  radiant_floor.o  \
	  CETC_plt_dummy.o SOFC_Annex42_dummy.o 

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS) 

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
climi.F:
	rm -f climi.F
	ln -s $(BLDdir)/climi.F .
cntmnt.F:
	rm -f cntmnt.F
	ln -s $(MFSdir)/cntmnt.F .
mstmsc.F:
	rm -f mstmsc.F
	ln -s $(BLDdir)/mstmsc.F .
montrb.F:
	rm -f montrb.F
	ln -s $(BLDdir)/montrb.F .
econtrol.F:
	rm -f econtrol.F
	ln -s $(COMdir)/econtrol.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
h3k_utils.F:
	rm -f h3k_utils.F
	ln -s $(CETCdir)/h3k_utils.F .
inital.F:
	rm -f inital.F
	ln -s $(BLDdir)/inital.F .
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
mfdump.F:
	rm -f mfdump.F
	ln -s $(MFSdir)/mfdump.F .
mfmach.F:
	rm -f mfmach.F
	ln -s $(MFSdir)/mfmach.F .
mfmode.F:
	rm -f mfmode.F
	ln -s $(MFSdir)/mfmode.F .
mfsbps.F:
	rm -f mfsbps.F
	ln -s $(MFSdir)/mfsbps.F .
mfsplt.F:
	rm -f mfsplt.F
	ln -s $(MFSdir)/mfsplt.F .
mfutil.F:
	rm -f mfutil.F
	ln -s $(MFSdir)/mfutil.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
precal.F:
	rm -f precal.F
	ln -s $(BLDdir)/precal.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(TDFdir)/tdfile.F .
tdrecbps.F:
	rm -f tdrecbps.F
	ln -s $(TDFdir)/tdrecbps.F .
# CETC files
radiant_floor.F:
	rm -f radiant_floor.F
	ln -s $(CETCdir)/radiant_floor.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	c2fdum.F climi.F cntmnt.F econtrol.F emfnetw.F h3k_utils.F inital.F initalc.F mfcdat.F \
	item.F mfcntl.F mfdump.F mfmach.F mfmode.F mfsbps.F mfsplt.F mfutil.F \
	pltcfg.F precal.F psychro.F startup.F tdfile.F tdrecbps.F \
	radiant_floor.F SOFC_Annex_42_dummy.F

