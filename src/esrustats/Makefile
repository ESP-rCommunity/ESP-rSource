SRCdir = `pwd`/..
LIBdir = $(SRCdir)/lib
PROG = stats$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = esru_ask.o esru_blk.o esru_fc.o esru_lib.o esru_libNonGTK.o csv_stats.o 

$(PROG): $(OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS)

esru_blk.F:
	rm -f esru_blk.F
	ln -s $(LIBdir)/esru_blk.F .
esru_fc.F:
	rm -f esru_fc.F
	ln -s $(LIBdir)/esru_fc.F .
esru_lib.F:
	rm -f esru_lib.F
	ln -s $(LIBdir)/esru_lib.F .
esru_libNonGTK.F:
	rm -f esru_libNonGTK.F
	ln -s $(LIBdir)/esru_libNonGTK.F .
esru_ask.F:
	rm -f esru_ask.F
	ln -s $(LIBdir)/esru_ask.F .

strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) \
	esru_ask.F esru_blk.F esru_fc.F esru_lib.F esru_libNonGTK.F 

