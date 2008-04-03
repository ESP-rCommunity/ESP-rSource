SHELL=/bin/sh
LIBGTK = libgtkesru.a
LIBX11 = libxesru.a
LIBnoX = libnoxesru.a

# .SUFFIXES: .o .F
# .F.o:
# 	$(MFC) $(LD_FLAGS) $(FFLAGS) -c $<
# 
# %.o : %.c
# 	$(MCC) $(CFLAGS) -c $<

OBJECTSX11 = esru_ask.o esru_blk.o esru_libX11.o esru_x.o
OBJECTSGTK = esru_ask.o esru_blk.o esru_libGTK.o esru_nox.o esru.o
OBJECTSnoX = esru_ask.o esru_blk.o esru_libX11.o esru_fc.o esru_nox.o
GTK_OBJ = esp-r.o esp_menu.o esp_text.o esp_ask.o esp_draw.o esp_file.o

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

clean:
	rm -f core $(LIBX11) $(LIBnoX) $(LIBGTK) $(OBJECTSX11) $(OBJECTSnoX) $(OBJECTSGTK) $(GTK_OBJ) 

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
esru_libX11.o:
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libX11.F
esru_nox.o:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c esru_nox.c
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
