SWTCH= -D
SRCdir = `pwd`/..
LIBdir = $(SRCdir)/lib
COMdir = $(SRCdir)/esrucom
PRJdir = $(SRCdir)/esruprj
PROG = stats$(EXE)
default: $(PROG)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c $<

OBJECTS = esru_ask.o esru_blk.o esru_fc.o esru_lib.o esru_libNonGTK.o csv_stats.o redir_console_output.o \
	rwsbem.o  disable_h3k.o

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
rwsbem.F:
	rm -f rwsbem.F
	ln -s $(COMdir)/rwsbem.F .
redir_console_output.o:
	rm -f redir_console_output.c
	ln -s $(LIBdir)/redir_console_output.c 
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c redir_console_output.c .

strip:
	strip $(PROG)

install:
	install $(SWTCH) $(PROG) $(ESPBinDir)/$(PROG)

clean:
	rm -f core $(PROG) $(PROG).exe $(OBJECTS) \
	esru_ask.F esru_blk.F esru_fc.F esru_lib.F esru_libNonGTK.F redir_console_output.c rwsbem.F  
 
distclean: clean

uninstall:
	-rm -f $(ESPBinDir)/$(PROG)
	
.PHONY: clean install default strip uninstall distclean
       
