SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = mfs$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

MODULE_OBJECTS = startup.o

OBJECTS = mfs.o c2fdum.o cntmnt.o ctlexp.o dossupport.o econtrol.o emfnetw.o \
	  fctlc.o gcfunc.o initalc.o mfcdat.o mfcntl.o mfdumb.o mfdumf.o mfdump.o \
	  mfinit.o mfmach.o mfmode.o mfplot.o mfrlst.o mfrset.o mftabl.o \
	  mfutil.o psychro.o rnor.o 

MODULES = start_up.mod
	  
$(PROG): $(MODULE_OBJECTS) $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
econtrol.F:
	rm -f econtrol.F
	ln -s $(COMdir)/econtrol.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
gcfunc.F:
	rm -f gcfunc.F
	ln -s $(COMdir)/gcfunc.F .
initalc.F:
	rm -f initalc.F
	ln -s $(COMdir)/initalc.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
rnor.F:
	rm -f rnor.F
	ln -s $(COMdir)/rnor.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(MODULE_OBJECTS) $(OBJECTS) $(MODULES) \
	c2fdum.F ctlexp.F dossupport.F econtrol.F emfnetw.F gcfunc.F \
        initalc.F psychro.F rnor.F startup.F 
