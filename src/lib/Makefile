SHELL=/bin/sh
SRCdir = `pwd`/..
CETCdir = $(SRCdir)/cetc
LIBGTK = libgtkesru.a
LIBX11 = libxesru.a
LIBnoX = libnoxesru.a
LIBver = libversion.a
LIBfpe = libfpe.a
LIBxml = libXML.a
LIBsqlite = libSQLite.a
# .SUFFIXES: .o .F
# .F.o:
# 	$(MFC) $(LD_FLAGS) $(FFLAGS) -c $<
# 
# %.o : %.c
# 	$(MCC) $(CFLAGS) -c $<

%.o : %.f90
	$(MCC) $(FFLAGS) -c $<


OBJECTSX11 = esru_ask.o esru_blk.o esru_lib.o esru_libNonGTK.o esru_libX11.o esru_x.o esru_util.o
OBJECTSGTK = esru_ask.o esru_blk.o esru_lib.o esru_libGTK.o esru_nox.o esru.o esru_util.o
OBJECTSnoX = esru_ask.o esru_blk.o esru_lib.o esru_libNonGTK.o esru_fc.o esru_nox.o esru_util.o
GTK_OBJ = esp-r.o esp_menu.o esp_text.o esp_ask.o esp_draw.o esp_file.o
OBJECTSVersion = version.o
OBJECTS_FPE_enable = fpe_enable.o
OBJECTS_FPE_disable = fpe_disable.o


# Additional C and Fortran objectives needed for H3Kreports
XML_COBJECTS = TBinnedData.o TXMLAdapter.o TWildCards.o TReportData.o TReportsManager.o
XML_FOBJECTS = h3kmodule.o ConfigureH3kReports.o 

# C and Fortran stubs used to build bps without H3Kreports
NO_XML_C_STUBS =
NO_XML_F_STUBS = TReports_dummy.o

# Additional C and Fortran objectives needed for SQLite
SQLite_COBJECTS = DBManager.o
SQLite_FOBJECTS = 

# C and Fortran stubs used to build bps without SQLite
NO_SQLite_C_STUBS = DBManager_stub.o
NO_SQLite_F_STUBS = 


GTKFLAGS = -I. -DG_DISABLE_DEPRECIATED -DGDK_DISABLE_DEPRECIATED \
	-DGDK_PIXBUF_DISABLE_DEPRECIATED -DGTK_DISABLE_DEPRECIATED \
        `pkg-config gtk+-2.0 --cflags`

the_libGTK:$(OBJECTSGTK) $(GTK_OBJ)
	ar ruv $(LIBGTK) $(OBJECTSGTK) $(GTK_OBJ)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBGTK) ; \
	fi

the_libX11: $(OBJECTSX11) 
	ar ruv $(LIBX11) $(OBJECTSX11) 
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBX11) ; \
	fi

the_libnoX: $(OBJECTSnoX) 
	ar ruv $(LIBnoX) $(OBJECTSnoX) 
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBnoX) ; \
	fi

the_libVer: $(OBJECTSVersion) 
	ar ruv $(LIBver) $(OBJECTSVersion)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBver) ; \
	fi

the_libFPE: $(OBJECTS_FPE_enable) 
	ar ruv $(LIBfpe) $(OBJECTS_FPE_enable)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBfpe) ; \
	fi

the_libNoFPE: $(OBJECTS_FPE_disable) 
	ar ruv $(LIBfpe) $(OBJECTS_FPE_disable)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBfpe) ; \
	fi

# Target to build bps with SQLite
the_libSQLite: $(SQLite_FOBJECTS) $(SQLite_COBJECTS)
	ar ruv $(LIBsqlite) $(SQLite_FOBJECTS) $(SQLite_COBJECTS)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBsqlite) ; \
	fi

# Target to build bps without SQLite
the_libNoSQLite: $(NO_SQLite_C_STUBS) $(NO_SQLite_F_STUBS)
	ar ruv $(LIBsqlite) $(NO_SQLite_C_STUBS) $(NO_SQLite_F_STUBS)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBsqlite) ; \
	fi

# Target to build bps with h3kreports
the_libH3KReports: $(XML_FOBJECTS) $(XML_COBJECTS)
	ar ruv $(LIBxml) $(XML_FOBJECTS) $(XML_COBJECTS)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBxml) ; \
	fi

# Target to build bps without h3kreports
the_libNoH3KReports: $(NO_XML_C_STUBS) $(NO_XML_F_STUBS)
	ar ruv $(LIBxml) $(NO_XML_C_STUBS) $(NO_XML_F_STUBS)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBxml) ; \
	fi

clean:
	rm -f core $(LIBX11) $(LIBnoX) $(LIBGTK) $(LIBver) $(LIBfpe) $(LIBxml) $(LIBsqlite)\
	           $(OBJECTSX11) $(OBJECTSnoX) $(OBJECTSGTK) $(GTK_OBJ) \
	           $(OBJECTSVersion) $(OBJECTS_FPE_enable) $(OBJECTS_FPE_disable) \
	           $(XML_COBJECTS) $(XML_FOBJECTS) $(NO_XML_C_STUBS) $(NO_XML_F_STUBS) \
		   $(SQLite_COBJECTS) $(SQLite_FOBJECTS) $(NO_SQLite_C_STUBS) $(NO_SQLite_F_STUBS) \
	           TReports_dummy.F ConfigureH3kReports.F TReportsManager.cpp \
	           TBinnedData.cpp TXMLAdapter.cpp TWildCards.cpp DBManager.cpp TReportData.cpp \
		   h3kmodule.f90 h3kmodule.mod DBManager_stub.cpp
# File-by-file build instructions:

esp-r.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp-r.c
esp_menu.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_menu.c
esp_text.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_text.c
esp_ask.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_ask.c
esp_draw.o:	
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_draw.c
esp_file.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_file.c
esru_lib.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_lib.F
esru_libX11.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libX11.F
esru_libNonGTK.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libNonGTK.F	
esru_nox.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c esru_nox.c
fpe_enable.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c fpe_enable.c
fpe_disable.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c fpe_disable.c
esru.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esru.c
esru_blk.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_blk.F
esru_libGTK.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libGTK.F
esru_ask.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_ask.F
esru_x.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -I$(XINSTALLDIR)/include -c esru_x.c
esru_fc.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_fc.f
esru_util.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c esru_util.c
version.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c version.F


# H3k-reports object files
TReportsManager.o:
	rm -f TReportsManager.cpp ;
	ln -s $(CETCdir)/h3kreports/TReportsManager.cpp .;
	$(MCPL) $(LD_FLAGS) $(CFLAGS) -c TReportsManager.cpp
TBinnedData.o:
	rm -f TBinnedData.cpp ;
	ln -s $(CETCdir)/h3kreports/TBinnedData.cpp .;
	$(MCPL) $(LD_FLAGS) $(CFLAGS) -c TBinnedData.cpp
TXMLAdapter.o:
	rm -f TXMLAdapter.cpp ;
	ln -s $(CETCdir)/h3kreports/TXMLAdapter.cpp . ;
	$(MCPL) $(LD_FLAGS) $(CFLAGS) -c TXMLAdapter.cpp
TWildCards.o:
	rm -f TWildCards.cpp ;
	ln -s $(CETCdir)/h3kreports/TWildCards.cpp . ;
	$(MCPL) $(LD_FLAGS) $(CFLAGS) -c TWildCards.cpp
TReports_dummy.o:
	rm -f TReports_dummy.F ;
	ln -s $(CETCdir)/h3kreports/TReports_dummy.F . ;
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c TReports_dummy.F
ConfigureH3kReports.o:
	rm -f ConfigureH3kReports.F ;	
	ln -s $(CETCdir)/h3kreports/ConfigureH3kReports.F . ;
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c ConfigureH3kReports.F
h3kmodule.o:
	rm -f h3kmodule.f90
	ln -s $(CETCdir)/h3kmodule.f90 . ;
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c h3kmodule.f90; 
DBManager.o:
	rm -f DBManager.cpp ;
	ln -s $(CETCdir)/h3kreports/DBManager.cpp .;
	$(MCPL) $(LD_FLAGS) $(CFLAGS) -c DBManager.cpp
DBManager_stub.o:
	rm -f DBManager_stub.cpp ;
	ln -s $(CETCdir)/h3kreports/DBManager_stub.cpp .;
	$(MCPL) $(LD_FLAGS) $(CFLAGS) -c DBManager_stub.cpp
TReportData.o:
	rm -f TReportData.cpp ;
	ln -s $(CETCdir)/h3kreports/TReportData.cpp .;
	$(MCPL) $(LD_FLAGS) $(CFLAGS) -c TReportData.cpp
