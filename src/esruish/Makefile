SRCdir = `pwd`/..
COMdir = $(SRCdir)/esrucom
PRJdir = $(SRCdir)/esruprj
PROG = ish$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = ish.o arrow.o c2fdum.o common3dv.o comput.o cread3dv.o dossupport.o e3dviews.o \
	  edatabase.o egeometry.o eroper.o esru_misc.o files.o graph.o inscom.o \
	  miscel.o plelev.o scsys.o shdcon.o silist.o sort.o startup.o \
	  synop.o tfiles.o trnsf.o trnsfi.o winshd.o wshad.o

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)

arrow.F:
	rm -f arrow.F
	ln -s $(PRJdir)/arrow.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
common3dv.F:
	rm -f common3dv.F
	ln -s $(COMdir)/common3dv.F .
cread3dv.F:
	rm -f cread3dv.F
	ln -s $(COMdir)/cread3dv.F .
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
plelev.F:
	rm -f plelev.F
	ln -s $(COMdir)/plelev.F .
scsys.F:
	rm -f scsys.F
	ln -s $(COMdir)/scsys.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .

install:
	strip $(PROG);cp $(PROG) $(ESPbin)

clean:
	rm -f core $(PROG) $(OBJECTS) \
	arrow.F c2fdum.F common3dv.F cread3dv.F dossupport.F e3dviews.F edatabase.F egeometry.F esru_misc.F \
	eroper.F plelev.F scsys.F sort.F startup.F 
