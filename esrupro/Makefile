SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PROG = pro$(EXE)


.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = pro.o c2fdum.o eprfilec.o progrf.o startup.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	c2fdum.F startup.F 
