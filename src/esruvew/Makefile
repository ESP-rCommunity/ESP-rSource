SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = viewer$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = startup.o viewer.o azalts.o berror.o beye.o bmatr.o c2fdum.o chat.o \
	  ctlexp.o dossupport.o ext.o hlr.o hlr2.o input.o invert.o \
	  pconvt.o pers1.o pers2.o plan.o plot.o select.o shrink.o \
	  spout.o  transf.o vert.o

MODULES = start_up.mod parse_command_line.mod

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
ctlexp.F:
	rm -f ctlexp.F
	ln -s $(COMdir)/ctlexp.F .
dossupport.F:
	rm -f dossupport.F
	ln -s $(COMdir)/dossupport.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) $(MODULES) \
	c2fdum.F ctlexp.F startup.F dossupport.F
