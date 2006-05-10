SHELL=/bin/sh
LIBGTK = libgtkesru.a
LIBX11 = libxesru.a

OBJECTSX11 = esru_ask.o esru_blk.o esru_libX11.o
OBJECTSGTK = esru_ask.o esru_blk.o esru_libGTK.o esru_nox.o esru.o
GTK_OBJ = esp-r.o esp_menu.o esp_text.o esp_ask.o esp_draw.o esp_file.o
C_OBJ = esru_x.o

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
	$(MCC) $(LD_FLAGS) $(CFLAGS) $(GTKFLAGS) -c esru.c
	$(MCC) $(LD_FLAGS) $(CFLAGS) -c esru_nox.c
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_blk.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libGTK.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_ask.F

sourcesX11:
	$(MCC) $(LD_FLAGS) $(CFLAGS) -I$(XINSTALLDIR)/include -c esru_x.c
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_blk.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_libX11.F
	$(MFC) $(LD_FLAGS) $(FFLAGS) -c esru_ask.F

the_libGTK: $(GTK_OBJ) $(OBJECTSGTK) 
	ar ruv $(LIBGTK) $(LD_FLAGS) $(OBJECTSGTK) $(GTK_OBJ)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBGTK) ; \
	fi

the_libX11: $(C_OBJ) $(OBJECTSX11) 
	ar ruv $(LIBX11) $(LD_FLAGS) $(OBJECTSX11) $(C_OBJ)
	if test -s /usr/bin/ranlib ; \
	then ranlib $(LIBX11) ; \
	fi

clean:
	rm -f core $(LIBX11) $(LIBGTK) $(OBJECTSX11) $(OBJECTSGTK) $(GTK_OBJ) $(C_OBJ)
