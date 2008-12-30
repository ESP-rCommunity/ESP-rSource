SRCdir = `pwd`/..
PROG=win32test$(EXE)

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -mwindows -c $<

%.o : %.cpp
	$(MCPL) $(CFLAGS) -mwindows -c $<

%.o : %.c
	$(MCC) $(CFLAGS) -mwindows -c $<

BOX_OBJECTS= complex_msg_box.o

$(PROG): $(BOX_OBJECTS)
	$(MCPL) $(LD_FLAGS) -mwindows -o $(PROG) $(FFLAGS) $(BOX_OBJECTS) $(ULIBS) $(F_to_C_flags)
	
#strip:
#	@if [ -f $(PROG) ]; then strip $(PROG); fi

#install:
#	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi
	
#clean:
#	rm -f core $(PROG) $(PROG).exe $(BPS_OBJECTS) 	
