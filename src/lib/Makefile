SHELL=/bin/sh
LIBGTK = libgtkesru.a
LIBX11 = libxesru.a
LIBnoX = libnoxesru.a

OBJECTSX11 = esru_ask.o esru_blk.o esru_libX11.o esru_x.o
OBJECTSGTK = esru_ask.o esru_blk.o esru_libGTK.o esru_nox.o esru.o
OBJECTSnoX = esru_ask.o esru_blk.o esru_libX11.o esru_fc.o esru_nox.o
GTK_OBJ = esp-r.o esp_menu.o esp_text.o esp_ask.o esp_draw.o esp_file.o

GTKFLAGS = -I. -DG_DISABLE_DEPRECIATED -DGDK_DISABLE_DEPRECIATED \
	-DGDK_PIXBUF_DISABLE_DEPRECIATED -DGTK_DISABLE_DEPRECIATED \
        `pkg-config gtk+-2.0 --cflags`

sourcesGTK:
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp-r.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_menu.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_text.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_ask.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_draw.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esp_file.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c esru_nox.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esru.c
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_blk.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libGTK.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_ask.F

sourcesX11:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -I$(XINSTALLDIR)/include -c esru_x.c
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_blk.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libX11.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_ask.F

sourcesnoX:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c esru_nox.c
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_fc.f
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_blk.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libX11.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_ask.F

the_libGTK:
	ar ruv $(LIBGTK) $(OBJECTSGTK) $(GTK_OBJ)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBGTK) ; \
	fi

the_libX11:
	ar ruv $(LIBX11) $(OBJECTSX11) 
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBX11) ; \
	fi

the_libnoX:
	ar ruv $(LIBnoX) $(OBJECTSnoX) 
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBnoX) ; \
	fi

clean:
	rm -f core $(LIBX11) $(LIBnoX) $(LIBGTK) $(OBJECTSX11) $(OBJECTSnoX) $(OBJECTSGTK) $(GTK_OBJ) 


