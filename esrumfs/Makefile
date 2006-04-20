SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
CTLdir = $(SRCdir)/esructl
PROG = mfs$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = mfs.o c2fdum.o cntmnt.o econtrol.o emfnetw.o fctlc.o initalc.o mfcdat.o \
	  mfcntl.o mfdumb.o mfdumf.o mfdump.o mfinit.o mfmach.o mfmode.o \
	  mfplot.o mfrlst.o mfrset.o mftabl.o mfutil.o psychro.o \
	  startup.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
econtrol.F:
	rm -f econtrol.F
	ln -s $(COMdir)/econtrol.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
initalc.F:
	rm -f initalc.F
	ln -s $(CTLdir)/initalc.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	c2fdum.F econtrol.F emfnetw.F initalc.F psychro.F startup.F 
