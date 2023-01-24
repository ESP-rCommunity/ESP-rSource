/* Functions to set up the main display window and
   divide it into frames: graphics, text, menu.

   Functions:
     wstxpt_() write a string beginning at pixel x and y.
     textatxywwc_() write a string to file at pixel x y in colour xcolid.
     textatxy_() write a string at pixel x y in colour xcolid.
     textsizeatxywwc_() write to file attributes of testsizeatxy.
     textsizeatxy_() write a string at pixel x y with size & in colour xcolid.
     textpixwidth_() find width of string (buff) in current font.
     viewtextwwc_() display text in graphics box
     viewtext_() display text in graphics box
     findviewtext_() find position of display text in graphics box
     drawswl() draw a line of width 1 between two points
     esymbolwwc_() symbol drawing routine.
     esymbol_() symbol drawing routine.
     elinewwc_() line drawing routine.
     eline_() line drawing routine.
     edline_() dotted line drawing routine.
     edlinewwc_() dotted line drawing routine.
     edash_() dashed line drawing routine.
     eswlinewwc_() single width line drawing routine.
     eswline_() single width line drawing routine.
     edwlinewwc_() double width line drawing routine.
     edwline_() double width line drawing routine.
     echainwwc_() chained line drawing routine.
     echain_() chained line drawing routine.
     drawdwl() draw a line of width 2 between two points
     drawvwl() draw a line of user defined width between two points
     drawddash() draw an double dashed line of user defined width between two points
     drawoodash() draw an on-off dashed line of user defined width between two points
     axiscale_() determine scaling ratios
     linescale_() store scaling parameters for lines
     etplot_() general line plotting.
     etplotwwc_() general line plotting.
     erectan_() rectangle drawing routine.
     egrbox_() grey box drawing routine.
     etriang_() triangle drawing routine.
     ecirc_() circle drawing routine.
     earc_() arc drawing routine.
     labelstr() generate a tic label
     etlabel_() display text as in old teklib tlabel
     vrtaxis_() draws a vertical axis (tic & labels on right or left side).
     horaxis_() construct and draw a horiz axis
     horaxishdw_() construct and draw a hour/day/week horizontal axis
     testcscale_() test drawing primitives & colors
*/

#include <stdio.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include "esp-r.h"
#include <commons.h>

extern FILE *wwc;
extern int  wwc_ok;   /* assume this set in esru_util.c */
extern int  wwc_macro;   /* assume this set in esru_util.c */

/* f_height and f_width which already declared globaly in esp-r.c are used here. */
extern gint f_height;
extern gint f_width;


/* ********* wstxptwwc_() write a string beginning at pixel x and y to file. ******* */
void wstxptwwc_(x,y,buff,len)
long int *x, *y;       /* x y is the position of the string */
char *buff;
int  len;        /* len is length passed from fortran */
{
 char buffer[248];
 const char* get_text;

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}


/* ********* wstxpt_() write a string beginning at pixel x and y. ******* */
/* Note the calling code assumes that the pixel is at the lower left
 * corner of the block of text. An internal adjustment is required
 * as Pango layout uses the upper left corner.
 */
void wstxpt_(x,y,buff,len)
char *buff;
long int *x, *y;       /* x y is the position of the string */
int  len;        /* len is length passed from fortran */
{
 char buffer[248];
 const char* get_text;
 gint ix = (gint) *x;
 gint iy = (gint) *y;
 PangoFontDescription *pfd;	/* to hold test font */
 PangoContext *context;	/* for use in finding font properties */
 PangoLayout *layout;	/* pango layout for the text in the buffer */

/* create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height. */
 if (butn_fnt == 4 ) {
   pfd = pango_font_description_from_string("Serif,Medium 8");
   f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
   f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
/*     g_print("viewtext graphic font medium 8\n");  debug */
 } else if (butn_fnt == 5 ) {
   pfd = pango_font_description_from_string("Serif,Medium 9");
   f_height = font_calculations_array[serif_medium].f_height;
   f_width  = font_calculations_array[serif_medium].f_width;
/*     g_print("viewtext graphic font medium 9\n");  debug */
 } else if (butn_fnt == 6 ) {
   pfd = pango_font_description_from_string("Serif,Medium 10");
   f_height = font_calculations_array[serif_large].f_height;
   f_width  = font_calculations_array[serif_large].f_width;
/*    g_print("viewtext graphic font medium 10\n");  debug */
 } else if (butn_fnt == 7 ) {
   pfd = pango_font_description_from_string("Serif,Medium 11");
   f_height = font_calculations_array[serif_largest].f_height;
   f_width  = font_calculations_array[serif_largest].f_width;
#/*   g_print("wstxpt graphic font serif medium 11\n");  debug */
 } else if (butn_fnt == 0 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 8");
/*   g_print("wstxpt graphic font Mono medium 8\n");  debug */
 } else if (butn_fnt == 1 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 9");
/*   g_print("wstxpt graphic font Mono medium 9\n");  debug */
 } else if (butn_fnt == 2 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 10");
/*   g_print("wstxpt graphic font Mono medium 10\n");  debug */
 } else if (butn_fnt == 3 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 11");
/*   g_print("wstxpt graphic font Mono medium 11\n");  debug */
}

 context = gtk_widget_get_pango_context (graphic);

 pango_font_description_free(pfd);

 layout = pango_layout_new (context);	/* clear and set layout */
 buffer[0] = '\0';
 g_snprintf (buffer, sizeof (buffer), "%s", buff); /* copy buff into buffer */

 pango_layout_set_text (layout, buffer, len);	/* add len char of text */

 get_text = pango_layout_get_text (layout);
 /* fprintf(stderr, "\nget_text displays: %s\n", buffer);   debug */

 /* draw it on the pixmap taking acount to shift text up by f_height. */
 gdk_draw_layout (gr_image, gc,ix,iy-f_height,layout);
 g_object_unref (layout);	/* clear the layout */

/*  g_print("wstxpt font height and width is %d %d  %s\n", f_height,f_width,buffer); debug */

/* If echo send parameters to wwc file
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
*/
 return;
}


/* ********* textatxywwc_() write a string at pixel x y in colour act & n. ******* */
void textatxywwc_(x,y,buff,act,n,len)
long int *x, *y;    /* x y is the position of the string */
char *buff;
char *act;          /* single character passed for colour set */
long int *n;        /* colour index within the set */
int  len;           /* len is length passed from fortran */
{
/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*textatxy\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}

/* ********* textatxy_() write a string at pixel x y in colour act & n. *******
This version, as opposed to textsizeatxy, should be used when the string needs
to be clipped. The font size should be set first using winfnt, then the fortran
soubroutine CLIPST should be called, then if required this subroutine will draw
the string. This process is appropriate where clipping is likely to be required
e.g. interactive 3D graphcs displays. */

void textatxy_(x,y,buff,act,n,len)
long int *x, *y;       /* x y is the position of the string */
char *buff;
char *act;  /* single character passed for colour set */
long int *n;       /* colour index within the set */
int  len;        /* len is length passed from fortran */
{
 PangoFontDescription *pfd;	/* to hold test font */
 PangoContext *context;	/* for use in finding font properties */
 PangoLayout *layout;	/* pango layout for the text in the buffer */
 gint ix = (gint) *x;
 gint iy = (gint) *y;
 int ilen;
 int ic;
 char buffer[248];

 ic = (int) *n;

/* create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height.*/
 if (butn_fnt == 4 ) {
   pfd = pango_font_description_from_string("Serif,Medium 8");
   f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
   f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
/*     g_print("viewtext graphic font medium 8\n");  debug */
 } else if (butn_fnt == 5 ) {
   pfd = pango_font_description_from_string("Serif,Medium 9");
   f_height = font_calculations_array[serif_medium].f_height;
   f_width  = font_calculations_array[serif_medium].f_width;
/*     g_print("viewtext graphic font medium 9\n");  debug */
 } else if (butn_fnt == 6 ) {
   pfd = pango_font_description_from_string("Serif,Medium 10");
   f_height = font_calculations_array[serif_large].f_height;
   f_width  = font_calculations_array[serif_large].f_width;
/*    g_print("viewtext graphic font medium 10\n");  debug */
 } else if (butn_fnt == 7 ) {
   pfd = pango_font_description_from_string("Serif,Medium 11");
   f_height = font_calculations_array[serif_largest].f_height;
   f_width  = font_calculations_array[serif_largest].f_width;

 } else if (butn_fnt == 0 ) {
  pfd = pango_font_description_from_string("Monospace,Medium 8");
   f_height = font_calculations_array[courier_small].f_height;
   f_width  = font_calculations_array[courier_small].f_width;
/*   g_print("wstxpt graphic font Mono medium 8\n");  debug */
 } else if (butn_fnt == 1 ) {
  pfd = pango_font_description_from_string("Monospace,Medium 9");
  f_height = font_calculations_array[courier_medium].f_height;
  f_width  = font_calculations_array[courier_medium].f_width;
/*   g_print("wstxpt graphic font Mono medium 9\n");  debug */
 } else if (butn_fnt == 2 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 10");
   f_height = font_calculations_array[courier_large].f_height;
   f_width  = font_calculations_array[courier_large].f_width;
/*   g_print("wstxpt graphic font Mono medium 10\n");  debug */
 } else if (butn_fnt == 3 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 11");
   f_height = font_calculations_array[courier_largest].f_height;
   f_width  = font_calculations_array[courier_largest].f_width;
/*   g_print("wstxpt graphic font Mono medium 11\n");  debug */
 }

 gtk_widget_modify_font(graphic, pfd);	/* << ?? >> */
 context = gtk_widget_get_pango_context (graphic);

 pango_font_description_free(pfd);

 layout = pango_layout_new (context);	/* clear and set layout */
 buffer[0] = '\0';
 g_snprintf (buffer, sizeof (buffer), "%s", buff); /* copy buff into buffer */
 pango_layout_set_text (layout, buffer, len);	/* add len chars of text */

/* sets the current forground colour n depending on which active colour set being
   used use the same logic as in winscl_(act,n) */
  if(*act == 'g') {
     if (ic >= 0 && ic <= (int) ngscale ) {
       gdk_gc_set_foreground(gc,&gscale[ic]);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == 'z') {
     if (ic >= 0 && ic <= (int) izc ) {
       gdk_gc_set_foreground(gc,&zscale[ic]);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == 'c') {
     if (ic >= 0 && ic <= (int) ncscale ) {
       gdk_gc_set_foreground(gc,&cscale[ic]);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == 'i') {
     if (ic >= 0 && ic <= (int) ngr ) {	/* including black and white */
       if (ic == 0) gdk_gc_set_foreground(gc,&gmenuhl);
       if (ic == 1) gdk_gc_set_foreground(gc,&gmodbg);
       if (ic == 2) gdk_gc_set_foreground(gc,&gpopfr);
       if (ic == 3) gdk_gc_set_foreground(gc,&gfeedfr);
       if (ic == 4) gdk_gc_set_foreground(gc,&ginvert);
       if (ic == 5) gdk_gc_set_foreground(gc,&grey50);
       if (ic == 6) gdk_gc_set_foreground(gc,&grey43);
       if (ic == 7) gdk_gc_set_foreground(gc,&black);
       if (ic == 8) gdk_gc_set_foreground(gc,&white);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == '-') {
     gdk_gc_set_foreground(gc,&black);
     gdk_gc_set_background(gc,&white);
  }

 gdk_draw_layout (gr_image, gc,ix,iy,layout);	/* draw it on the pixmap */
 g_object_unref (layout);	/* clear the layout */
 gdk_gc_set_foreground(gc,&black);


/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*textatxy\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}

/* ********* textsizeatxywwc_() write a string of size at pixel to file. ******* */
void textsizeatxywwc_(x,y,buff,size,act,n,len)
long int *x, *y;       /* x y is the position of the string */
char *buff;
long int *size;    /* font size indicator (see below) */
char *act;  /* single character passed for colour set */
long int *n;       /* colour index within the set */
int  len;        /* len is length passed from fortran */
{
 int ilen;

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*textatxy\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}


/* ********* textsizeatxy_() write a string of size at pixel x y in colour act & n. *******
This version, as opposed to texteatxy, should be used when the string does not need to be
clipped. No preceding call to winfnt is required. This is appropriate where clipping is not
likely to be required e.g. drawing graphs. */

void textsizeatxy_(x,y,buff,size,act,n,len)
long int *x, *y;       /* x y is the position of the string */
char *buff;
long int *size;    /* font size indicator (see below) */
char *act;  /* single character passed for colour set */
long int *n;       /* colour index within the set */
int  len;        /* len is length passed from fortran */
{
 long int fsize;
 PangoFontDescription *pfd;	/* to hold test font */
 PangoContext *context;	/* for use in finding font properties */
 PangoLayout *layout;	/* pango layout for the text in the buffer */
 gint ix = (gint) *x;
 gint iy = (gint) *y;
 int ilen;
 int ic;
 char buffer[248];

 ic = (int) *n;
 fsize = *size;

/* create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height. */
 if (butn_fnt == 0 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 8");
     f_height = font_calculations_array[courier_small].f_height;
     f_width  = font_calculations_array[courier_small].f_width;
 } else if (butn_fnt == 1 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 9");
     f_height = font_calculations_array[courier_medium].f_height;
     f_width  = font_calculations_array[courier_medium].f_width;
 } else if (butn_fnt == 2 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 10");
     f_height = font_calculations_array[courier_large].f_height;
     f_width  = font_calculations_array[courier_large].f_width;
 } else if (butn_fnt == 3 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 11");
     f_height = font_calculations_array[courier_largest].f_height;
     f_width  = font_calculations_array[courier_largest].f_width;
 } else if (butn_fnt == 4 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
     f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
 } else if (butn_fnt == 5 ) {
     pfd = pango_font_description_from_string("Serif,Medium 9");
     f_height = font_calculations_array[serif_medium].f_height;
     f_width  = font_calculations_array[serif_medium].f_width;
 } else if (butn_fnt == 6 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     f_height = font_calculations_array[serif_large].f_height;
     f_width  = font_calculations_array[serif_large].f_width;
 } else if (butn_fnt == 7 ) {
     pfd = pango_font_description_from_string("Serif,Medium 11");
     f_height = font_calculations_array[serif_largest].f_height;
     f_width  = font_calculations_array[serif_largest].f_width;
 }

 gtk_widget_modify_font(graphic, pfd);	/* << ?? >> */
 context = gtk_widget_get_pango_context (graphic);

 pango_font_description_free(pfd);

 layout = pango_layout_new (context);	/* clear and set layout */
 buffer[0] = '\0';
 g_snprintf (buffer, sizeof (buffer), "%s", buff); /* copy buff into buffer */
 pango_layout_set_text (layout, buffer, len);	/* add len chars of text */

/* sets the current forground colour n depending on which active colour set being
   used use the same logic as in winscl_(act,n) */
  if(*act == 'g') {
     if (ic >= 0 && ic <= (int) ngscale ) {
       gdk_gc_set_foreground(gc,&gscale[ic]);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == 'z') {
     if (ic >= 0 && ic <= (int) izc ) {
       gdk_gc_set_foreground(gc,&zscale[ic]);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == 'c') {
     if (ic >= 0 && ic <= (int) ncscale ) {
       gdk_gc_set_foreground(gc,&cscale[ic]);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == 'i') {
     if (ic >= 0 && ic <= (int) ngr ) {	/* including black and white */
       if (ic == 0) gdk_gc_set_foreground(gc,&gmenuhl);
       if (ic == 1) gdk_gc_set_foreground(gc,&gmodbg);
       if (ic == 2) gdk_gc_set_foreground(gc,&gpopfr);
       if (ic == 3) gdk_gc_set_foreground(gc,&gfeedfr);
       if (ic == 4) gdk_gc_set_foreground(gc,&ginvert);
       if (ic == 5) gdk_gc_set_foreground(gc,&grey50);
       if (ic == 6) gdk_gc_set_foreground(gc,&grey43);
       if (ic == 7) gdk_gc_set_foreground(gc,&black);
       if (ic == 8) gdk_gc_set_foreground(gc,&white);
     } else {
       gdk_gc_set_foreground(gc,&black);
     }
  } else if(*act == '-') {
     gdk_gc_set_foreground(gc,&black);
     gdk_gc_set_background(gc,&white);
  }

 gdk_draw_layout (gr_image, gc,ix,iy,layout);	/* draw it on the pixmap */
 g_object_unref (layout);	/* clear the layout */
 gdk_gc_set_foreground(gc,&black);


/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*textatxy\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}

/* ********* textpixwidth_() find width of string (buff) in current font. ******* */
/* << note - should use the actual font - so it needs to know if this is for
 * << for a graphic or text or menu context. still TO BE DONE. */
void textpixwidth_(buff,pixelwidth,pixelheight,len)
char *buff;
long int *pixelwidth;	/* width of the string in pixels */
long int *pixelheight;	/* height of the string in pixels */
int  len;        /* len is length passed from fortran */
{
 PangoFontDescription *pfd;	/* to hold test font */
 PangoContext *context;	/* for use in finding font properties */
 PangoLayout *layout;	/* pango layout for the text in the buffer */
 PangoRectangle logical_rect;
 gint ilen;
 gint vfw;
 char buffer[248];
 gint width;	/* to hold initial smaller guess of width */

/* find number of characters in buff, load metrics for current font and
   the find the pixel width. */
 ilen = 0;
 f_to_c_l(buff,&len,&ilen);

/* create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height. */
 if (butn_fnt == 0 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 8");
     f_height = font_calculations_array[courier_small].f_height;
     f_width  = font_calculations_array[courier_small].f_width;
/*     g_print("viewtext graphic font medium 8\n");  debug */
 } else if (butn_fnt == 1 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 9");
     f_height = font_calculations_array[courier_medium].f_height;
     f_width  = font_calculations_array[courier_medium].f_width;
/*     g_print("viewtext graphic font medium 9\n");  debug */
 } else if (butn_fnt == 2 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 10");
     f_height = font_calculations_array[courier_large].f_height;
     f_width  = font_calculations_array[courier_large].f_width;
/*    g_print("viewtext graphic font medium 10\n");  debug */
 } else if (butn_fnt == 3 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 11");
     f_height = font_calculations_array[courier_largest].f_height;
     f_width  = font_calculations_array[courier_largest].f_width;
 } else if (butn_fnt == 4 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
     f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
/*    g_print("wstxpt graphic font medium 8\n");  debug */
 } else if (butn_fnt == 5 ) {
     pfd = pango_font_description_from_string("Serif,Medium 9");
     f_height = font_calculations_array[serif_medium].f_height;
     f_width  = font_calculations_array[serif_medium].f_width;
/*    g_print("wstxpt graphic font medium 9\n");  debug */
 } else if (butn_fnt == 6 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     f_height = font_calculations_array[serif_large].f_height;
     f_width  = font_calculations_array[serif_large].f_width;
/*    g_print("wstxpt graphic font medium 10\n");  debug */
 } else if (butn_fnt == 7 ) {
     pfd = pango_font_description_from_string("Serif,Medium 11");
     f_height = font_calculations_array[serif_largest].f_height;
     f_width  = font_calculations_array[serif_largest].f_width;
/*    g_print("wstxpt graphic font medium 11\n");  debug */
 }
 gtk_widget_modify_font(graphic, pfd);  /* << ?? >> */
 context = gtk_widget_get_pango_context (graphic);

 pango_font_description_free(pfd);

 width = f_width * ilen; /* conservative width */
 layout = pango_layout_new (context);  /* clear and set layout */
 buffer[0] = '\0';
 g_snprintf (buffer, sizeof (buffer), "%s", buff); /* copy buff into buffer */
 pango_layout_set_text (layout, buffer, len);  /* add len char of text */
 pango_layout_get_extents (layout, NULL, &logical_rect);  /* find box the text fits within */
 vfw = MAX (width, PANGO_PIXELS (logical_rect.width));
 if (vfw > 1 ) *pixelwidth = (long int) vfw;
 if (vfw > 1 ) *pixelheight = (long int) f_height;
/* debug g_print("phrase %s is %d %d %d pixels wide\n",buffer,vfw,width,PANGO_PIXELS (logical_rect.width)); */
 g_object_unref (layout);  /* clear the layout */

 return;
}

/* ************** viewtextwwc_() display text in graphics box to file *************** */
/*
 Version to write to file.
*/
void viewtextwwc_(msg,linep,side,size,len)
  char  *msg;                    /* character string  */
  long int *linep, *side, *size;     /* position indicators */
  int len;                 /* length from f77   */
{
  int t_len;
  t_len = 0;
  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*viewtext\n");
    fprintf(wwc,"%ld %ld %ld\n",*linep,*side,*size);
    fprintf(wwc,"%s\n",msg);
  }
  return;
} /* viewtextwwc  */


/* ************** viewtext_() display text in graphics box *************** */
/*
 Given a string 'msg' and the 'line' where the string should be written
 and whether it should be left (side = 0), centered (side = 1), or
 right justified (side = 2) as well as the font size (0=small, 1 2 =medium,
 3=large).
*/
void viewtext_(msg,linep,side,size,len)
  char  *msg;                    /* character string  */
  long int *linep, *side, *size;     /* position indicators */
  int len;                 /* length from f77   */
{
  PangoFontDescription *pfd;	/* to hold test font */
  PangoContext *context;	/* for use in finding font properties */
  PangoFontMetrics *metrics;
  PangoLayout *layout;	/* pango layout for the text in the buffer */
  PangoRectangle logical_rect;
  gint ix,iy,mid,fitpix;
  int t_len;	/* to remember the truncated length of msg */
  long int fsize;
  gint b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
  gint width;	/* conservative width of msg */
  char buffer[248];

  t_len = 0;
  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*viewtext\n");
    fprintf(wwc,"%ld %ld %ld\n",*linep,*side,*size);
    fprintf(wwc,"%s\n",msg);
  }

/* create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height.  */
  if (butn_fnt == 0 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 8");
     f_height = font_calculations_array[courier_small].f_height;
     f_width  = font_calculations_array[courier_small].f_width;
/*     g_print("viewtext graphic font medium 8\n");  debug */
  } else if (butn_fnt == 1 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 9");
     f_height = font_calculations_array[courier_medium].f_height;
     f_width  = font_calculations_array[courier_medium].f_width;
/*     g_print("viewtext graphic font medium 9\n");  debug */
  } else if (butn_fnt == 2 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 10");
     f_height = font_calculations_array[courier_large].f_height;
     f_width  = font_calculations_array[courier_large].f_width;
/*    g_print("viewtext graphic font medium 10\n");  debug */
  } else if (butn_fnt == 3 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 11");
     f_height = font_calculations_array[courier_largest].f_height;
     f_width  = font_calculations_array[courier_largest].f_width;
  } else if (butn_fnt == 4 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
     f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
/*    g_print("wstxpt graphic font medium 8\n");  debug */
  } else if (butn_fnt == 5 ) {
     pfd = pango_font_description_from_string("Serif,Medium 9");
     f_height = font_calculations_array[serif_medium].f_height;
     f_width  = font_calculations_array[serif_medium].f_width;
/*    g_print("wstxpt graphic font medium 9\n");  debug */
  } else if (butn_fnt == 6 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     f_height = font_calculations_array[serif_large].f_height;
     f_width  = font_calculations_array[serif_large].f_width;
/*    g_print("wstxpt graphic font medium 10\n");  debug */
  } else if (butn_fnt == 7 ) {
     pfd = pango_font_description_from_string("Serif,Medium 11");
     f_height = font_calculations_array[serif_largest].f_height;
     f_width  = font_calculations_array[serif_largest].f_width;
/*    g_print("wstxpt graphic font medium 11\n");  debug */
  }
  gtk_widget_modify_font(graphic, pfd);  /* << ?? >> */
  context = gtk_widget_get_pango_context (graphic);

  pango_font_description_free(pfd);

  width = f_width * t_len; /* conservative width */
  layout = pango_layout_new (context);  /* clear and set layout */
  buffer[0] = '\0';
  g_snprintf (buffer, sizeof (buffer), "%s", msg); /* copy msg into buffer */

  pango_layout_set_text (layout, buffer, t_len);  /* add t_len char of text */
  pango_layout_get_extents (layout, NULL, &logical_rect);  /* find box the text fits within */
  fitpix = MAX (width, PANGO_PIXELS (logical_rect.width));

  fsize = *size;
  mid = 0 + ((graphic->allocation.width)/2);

  b_top = 0; b_left = 0; b_right = graphic->allocation.width; b_bottom = graphic->allocation.height;
  iy = b_top + 3 + (f_height * ((gint) *linep -1));  /* gdk_draw_layout works with top of font so subtract one line */
  if (*side == 0) {
      ix = b_left + 7;
  } else if (*side == 1) {
      ix = mid - (f_width * t_len / 2);
  } else if (*side == 2) {
      ix = b_right - (f_width * (t_len+1));
  } else {
      ix = b_left + 7;
  }
  gdk_draw_layout (gr_image, gc,ix,iy,layout);  /* draw it on the pixmap */
  g_object_unref (layout);  /* clear the layout */
  return;
} /* viewtext  */

/* *********** findviewtext_() find position of display text in graphics box *************** */
/*
 Given a character position 'charpos' and the 'line' where the string should be written
 and return the pixel position that XDrawString should use.
*/
void findviewtext_(charposp,linep,size,irx,iry)
  long int *charposp, *linep, *size, *irx, *iry;     /* position indicators */
{
  PangoFontDescription *pfd;	/* to hold test font */
  PangoContext *context;	/* for use in finding font properties */
  PangoFontMetrics *metrics;
  gint mid;
  long int fsize, charpos;

  gint b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
  gint width;	/* conservative width of msg */

/* << need to take into account the size parameter >> */

/* create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height. */
 if (butn_fnt == 0 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 8");
     f_height = font_calculations_array[courier_small].f_height;
     f_width  = font_calculations_array[courier_small].f_width;
/*     g_print("viewtext graphic font medium 8\n");  debug */
 } else if (butn_fnt == 1 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 9");
     f_height = font_calculations_array[courier_medium].f_height;
     f_width  = font_calculations_array[courier_medium].f_width;
/*     g_print("viewtext graphic font medium 9\n");  debug */
 } else if (butn_fnt == 2 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 10");
     f_height = font_calculations_array[courier_large].f_height;
     f_width  = font_calculations_array[courier_large].f_width;
/*    g_print("viewtext graphic font medium 10\n");  debug */
 } else if (butn_fnt == 3 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 11");
     f_height = font_calculations_array[courier_largest].f_height;
     f_width  = font_calculations_array[courier_largest].f_width;
 } else if (butn_fnt == 4 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
     f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
/*    g_print("wstxpt graphic font medium 8\n");  debug */
 } else if (butn_fnt == 5 ) {
     pfd = pango_font_description_from_string("Serif,Medium 9");
     f_height = font_calculations_array[serif_medium].f_height;
     f_width  = font_calculations_array[serif_medium].f_width;
/*    g_print("wstxpt graphic font medium 9\n");  debug */
 } else if (butn_fnt == 6 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     f_height = font_calculations_array[serif_large].f_height;
     f_width  = font_calculations_array[serif_large].f_width;
/*    g_print("wstxpt graphic font medium 10\n");  debug */
 } else if (butn_fnt == 7 ) {
     pfd = pango_font_description_from_string("Serif,Medium 11");
     f_height = font_calculations_array[serif_largest].f_height;
     f_width  = font_calculations_array[serif_largest].f_width;
/*    g_print("wstxpt graphic font medium 11\n");  debug */
}
  gtk_widget_modify_font(graphic, pfd);  /* << ?? >> */
  context = gtk_widget_get_pango_context (graphic);

  pango_font_description_free(pfd);

  b_top = 0; b_left = 0; b_right = graphic->allocation.width; b_bottom = graphic->allocation.height;

  fsize = *size;
  charpos = *charposp;
  mid = b_left + ((graphic->allocation.width)/2);

  *iry = b_top + 3 + (f_height * *linep);	/* y pixel at base of font */
  *irx = b_left + 7 + (f_width * charpos);	/* x pixel at left of font (always have 7 pixels) */
  return;
} /* findviewtext  */



/* ***** drawswl() draw a line of width 1 between two points **** */
void drawswl(xa,ya,xb,yb)
  gint xa,ya,xb,yb;
{
  gint width = 1;
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,xa,ya,xb,yb);
  return;
}


/* *************** esymbolwwc_() symbol drawing routine to file. *************** */
/*
 esymbolwwc version to write to file.
*/
void esymbolwwc_(x,y,sym,size)
  long int *x, *y, *sym, *size;
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*esymbol\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x,*y,*sym,*size);
  }
  return;
}

/* *************** esymbol_() symbol drawing routine. *************** */
/*
 esymbol is passed a pixel coord, a symbol index, and a size.
 currently there are 38 symbols.
*/
void esymbol_(x,y,sym,size)
  long int *x, *y, *sym, *size;
{
  gint isym,isize,ix,iy,width;
  GdkPoint p[13];
  ix = (gint) *x;
  iy = (gint) *y;
  isym = (gint) *sym;
  isize = (gint) *size;
  width = 1;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*esymbol\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x,*y,*sym,*size);
  }

/* debug fprintf(stderr,"%ld %ld %d %d  %ld %d  %ld\n",*x,*y,ix,iy,*sym,isym,*size); */
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */

/* initial values to zero */
  p[0].x=0; p[0].y=0; p[1].x=0; p[1].y=0;
  p[2].x=0; p[2].y=0; p[3].x=0; p[3].y=0;
  p[4].x=0; p[4].y=0; p[5].x=0; p[5].y=0;
  p[6].x=0; p[6].y=0; p[7].x=0; p[7].y=0;
  p[8].x=0; p[8].y=0; p[9].x=0; p[9].y=0;
  p[10].x=0; p[10].y=0; p[11].x=0; p[11].y=0;
  p[12].x=0; p[12].y=0;

  switch ( isym ) {    /* pick the appropriate symbol */
    case 0 :	       /* box 4 pixels wide and high */
      p[0].x=ix-2; p[0].y=iy-2; p[1].x=ix+2; p[1].y=iy-2;
      p[2].x=ix+2; p[2].y=iy+2; p[3].x=ix-2; p[3].y=iy+2;
      p[4].x=ix-2; p[4].y=iy-2;
      gdk_draw_lines(gr_image,gc,p,5); break;
    case 1 :  		/* triangle p[].x= ; p[].y= ; */
      p[0].x=ix; p[0].y=iy-3; p[1].x=ix+2; p[1].y=iy+2;
      p[2].x=ix-2; p[2].y=iy+2; p[3].x=ix; p[3].y=iy-3;
      gdk_draw_lines(gr_image,gc,p,4); break;
    case 2 : 		/* upside down triangle */
      p[0].x=ix; p[0].y=iy+3; p[1].x=ix+2; p[1].y=iy-2;
      p[2].x=ix-2; p[2].y=iy-2; p[3].x=ix; p[3].y=iy+3;
      gdk_draw_lines(gr_image,gc,p,4); break;
    case 3 : 		/* diamond */
      p[0].x=ix; p[0].y=iy-3; p[1].x=ix+3; p[1].y=iy;
      p[2].x=ix; p[2].y=iy+3; p[3].x=ix-3; p[3].y=iy;
      p[4].x=ix; p[4].y=iy-3; p[5].x=ix; p[5].y=iy-3;
      gdk_draw_lines(gr_image,gc,p,6); break;
    case 4 : 		/* lower right triangle */
      p[0].x=ix-3; p[0].y=iy+2; p[1].x=ix+2; p[1].y=iy+2;
      p[2].x=ix+2; p[2].y=iy-3; p[3].x=ix-3; p[3].y=iy+2;
      gdk_draw_lines(gr_image,gc,p,4); break;
    case 5 : 		/* lower left triangle */
      p[0].x=ix-2; p[0].y=iy+2; p[1].x=ix+3; p[1].y=iy+2;
      p[2].x=ix-2; p[2].y=iy-3; p[3].x=ix-2; p[3].y=iy+2;
      gdk_draw_lines(gr_image,gc,p,4); break;
    case 6 : 		/* upper left triangle */
      p[0].x=ix-2; p[0].y=iy+3; p[1].x=ix+3; p[1].y=iy-2;
      p[2].x=ix-2; p[2].y=iy-2; p[3].x=ix-2; p[3].y=iy+3;
      gdk_draw_lines(gr_image,gc,p,4); break;
    case 7 : 		/* upper right triangle */
      p[0].x=ix+2; p[0].y=iy+3; p[1].x=ix+2; p[1].y=iy-2;
      p[2].x=ix-3; p[2].y=iy-2; p[3].x=ix+2; p[3].y=iy+3;
      gdk_draw_lines(gr_image,gc,p,4); break;
    case 8 : 		
      if(isize==1) {         /* dot 2 pix wide, 3 pix high */
        drawswl(ix-1,iy-1,ix+1,iy-1);
        drawswl(ix-1,iy,ix+1,iy);
        drawswl(ix-1,iy+1,ix+1,iy+1);
      } else if(isize==0) {  /* dot 2 pix wide, 2 pix high (32) */
        drawswl(ix-1,iy-1,ix+1,iy-1);
        drawswl(ix-1,iy,ix+1,iy);
      } else if(isize==2) { /* dot 4 pix wide, 4 pix high (33) */
        drawswl(ix-2,iy-2,ix+2,iy-2);
        drawswl(ix-2,iy-1,ix+2,iy-1);
        drawswl(ix-2,iy,ix+2,iy);
        drawswl(ix-2,iy+1,ix+2,iy+1);
      }  break;
    case 9 : 		/* X */
      drawswl(ix-2,iy-2,ix+3,iy+3);
      drawswl(ix-2,iy+3,ix+3,iy-2);  break;
    case 10 : 		/* * asterick */
      drawswl(ix-2,iy-2,ix+3,iy+3);
      drawswl(ix-2,iy+3,ix+3,iy-2);
      drawswl(ix-2,iy,ix+3,iy);  break;
    case 11 : 		/* + */
      drawswl(ix-2,iy,ix+3,iy);
      drawswl(ix,iy-2,ix,iy+3);  break;
    case 12 : 		/* ^ up arrow */
      p[0].x=ix-2; p[0].y=iy+2; p[1].x=ix; p[1].y=iy-2;
      p[2].x=ix+3; p[2].y=iy+3;
      gdk_draw_lines(gr_image,gc,p,3); break;
    case 13 : 		/* v down arrow */
      p[0].x=ix-2; p[0].y=iy-2; p[1].x=ix; p[1].y=iy+2;
      p[2].x=ix+3; p[2].y=iy-3;
      gdk_draw_lines(gr_image,gc,p,3); break;
    case 14 : 		/* > left arrow */
      p[0].x=ix-2; p[0].y=iy+2; p[1].x=ix+2; p[1].y=iy;
      p[2].x=ix-3; p[2].y=iy-3;
      gdk_draw_lines(gr_image,gc,p,3); break;
    case 15 : 		/* < right arrow */
      p[0].x=ix+2; p[0].y=iy+2; p[1].x=ix-2; p[1].y=iy;
      p[2].x=ix+3; p[2].y=iy-3;
      gdk_draw_lines(gr_image,gc,p,3); break;
    case 16 : 		/* big dot with vertical line */
      drawswl(ix,iy-3,ix,iy+3);
      drawswl(ix-1,iy-1,ix+2,iy-1);
      drawswl(ix-1,iy,ix+2,iy);
      drawswl(ix-1,iy+1,ix+2,iy+1);  break;
    case 17 : 		/* dot with horizontal line */
      drawswl(ix-3,iy,ix+3,iy);
      drawswl(ix-1,iy-1,ix+1,iy-1);
      drawswl(ix-1,iy,ix+1,iy);
      drawswl(ix-1,iy+1,ix+1,iy+1);  break;
    case 18 :  		/* solid triangle  */
      drawswl(ix,iy-3,ix,iy+2);
      drawswl(ix-1,iy+2,ix-1,iy-1);
      drawswl(ix+1,iy+2,ix+1,iy-1);
      drawswl(ix,iy-3,ix+2,iy+2);
      drawswl(ix+2,iy+2,ix-2,iy+2);
      drawswl(ix-2,iy+2,ix,iy-3);  break;
    case 19 : 		/* solid upside down triangle */
      drawswl(ix,iy-2,ix,iy+3);
      drawswl(ix-1,iy-2,ix-1,iy+1);
      drawswl(ix+1,iy-2,ix+1,iy+1);
      drawswl(ix,iy+3,ix+2,iy-2);
      drawswl(ix+2,iy-2,ix-2,iy-2);
      drawswl(ix-2,iy-2,ix,iy+3);  break;
    case 20 : 		/* solid lower right triangle */
      drawswl(ix-1,iy+1,ix+2,iy+1);
      drawswl(ix,iy,ix+2,iy);
      drawswl(ix+1,iy-1,ix+2,iy-1);
      drawswl(ix-3,iy+2,ix+2,iy+2);
      drawswl(ix+2,iy+2,ix+2,iy-3);
      drawswl(ix+2,iy-3,ix-3,iy+2);  break;
    case 21 : 		/* solid lower left triangle */
      drawswl(ix-2,iy+1,ix+2,iy+1);
      drawswl(ix-2,iy,ix+1,iy);
      drawswl(ix-2,iy-1,ix+1,iy-1);
      drawswl(ix-2,iy+2,ix+3,iy+2);
      drawswl(ix+3,iy+2,ix-2,iy-3);
      drawswl(ix-2,iy-3,ix-2,iy+2);  break;
    case 22 : 		/* solid upper left triangle */
      drawswl(ix-2,iy+1,ix,iy+1);
      drawswl(ix-2,iy,ix+1,iy);
      drawswl(ix-2,iy-1,ix+2,iy-1);
      drawswl(ix-2,iy+3,ix+3,iy-2);
      drawswl(ix+3,iy-2,ix-2,iy-2);
      drawswl(ix-2,iy-2,ix-2,iy+3);  break;
    case 23 : 		/* solid upper right triangle */
      drawswl(ix-1,iy-1,ix+2,iy-1);
      drawswl(ix,iy,ix+2,iy);
      drawswl(ix+1,iy+1,ix+2,iy+1);
      drawswl(ix+2,iy+3,ix+2,iy-2);
      drawswl(ix+2,iy-2,ix-3,iy-2);
      drawswl(ix-3,iy-2,ix+2,iy+3);  break;
    case 24 : 		/* solid diamond */
      drawswl(ix,iy-3,ix,iy+3);
      drawswl(ix-3,iy,ix+3,iy);
      drawswl(ix-1,iy-1,ix-1,iy+2);
      drawswl(ix+1,iy-1,ix+1,iy+2);
      drawswl(ix,iy-3,ix+3,iy);
      drawswl(ix+3,iy,ix,iy+3);
      drawswl(ix,iy+3,ix-3,iy);
      drawswl(ix-3,iy,ix,iy-3);  break;
    case 25 :	       /* box 5 pixels wide and high with \ */
      drawswl(ix-2,iy-2,ix+3,iy+3);
      drawswl(ix-2,iy-2,ix+3,iy-2);
      drawswl(ix+3,iy-2,ix+3,iy+3);
      drawswl(ix+3,iy+3,ix-2,iy+3);
      drawswl(ix-2,iy+3,ix-2,iy-2);  break;
    case 26 : 		/* diamond with vertical line */
      drawswl(ix,iy-3,ix,iy+3);
      drawswl(ix,iy-3,ix+3,iy);
      drawswl(ix+3,iy,ix,iy+3);
      drawswl(ix,iy+3,ix-3,iy);
      drawswl(ix-3,iy,ix,iy-3);  break;
    case 27 :	       /* box 5 pixels wide and high with / */
      drawswl(ix-2,iy+3,ix+3,iy-2);
      drawswl(ix-2,iy-2,ix+3,iy-2);
      drawswl(ix+3,iy-2,ix+3,iy+3);
      drawswl(ix+3,iy+3,ix-2,iy+3);
      drawswl(ix-2,iy+3,ix-2,iy-2);  break;
    case 28 : 		/* diamond with horizontal line */
      drawswl(ix-3,iy,ix+3,iy);
      drawswl(ix,iy-3,ix+3,iy);
      drawswl(ix+3,iy,ix,iy+3);
      drawswl(ix,iy+3,ix-3,iy);
      drawswl(ix-3,iy,ix,iy-3);  break;
    case 29 : 		/* wireframe image control symbol */
      drawswl(ix-12,iy+2,ix-6,iy+5);	/* bounds of box */
      drawswl(ix-6,iy+5,ix-1,iy+1);
      drawswl(ix-1,iy+1,ix-1,iy-3);
      drawswl(ix-1,iy-3,ix-5,iy-5);
      drawswl(ix-6,iy-5,ix-12,iy-2);
      drawswl(ix-12,iy-2,ix-12,iy+2);
      drawswl(ix-6,iy+5,ix-6,iy);	/* inside edges */
      drawswl(ix-6,iy,ix-12,iy-2);
      drawswl(ix-6,iy,ix-2,iy-2);
      drawswl(ix+6,iy+1,ix+12,iy-4);	/* eye point */
      drawswl(ix+12,iy-4,ix+3,iy-7);
      drawswl(ix+7,iy-5,ix+6,iy-4);
      drawswl(ix+6,iy-4,ix+6,iy-2);
      drawswl(ix+6,iy-2,ix+7,iy);
      drawswl(ix+7,iy-3,ix+7,iy-1);  break;
    case 30 : 		/* > closed left arrow */
      drawswl(ix-3,iy+2,ix+2,iy);
      drawswl(ix+2,iy,ix-3,iy-3);
      drawswl(ix-3,iy-3,ix-3,iy+2);  break;
    case 31 : 		/* < closed right arrow */
      drawswl(ix+3,iy+2,ix-2,iy);
      drawswl(ix-2,iy,ix+3,iy-3);
      drawswl(ix+3,iy-3,ix+3,iy+2);  break;
    case 32 : 		/* dot 2 pix wide, 2 pix high */
      drawswl(ix-1,iy-1,ix+1,iy-1);
      drawswl(ix-1,iy,ix+1,iy); break;
    case 33 : 		/* dot 4 pix wide, 4 pix high */
      drawswl(ix-2,iy-2,ix+2,iy-2);
      drawswl(ix-2,iy-1,ix+2,iy-1);
      drawswl(ix-2,iy,ix+2,iy);
      drawswl(ix-2,iy+1,ix+2,iy+1); break;
    case 34 : 		/* flow crack --\/\--*/
      drawdwl(ix-8,iy,ix-3,iy);
      drawdwl(ix-3,iy,ix,iy-4);
      drawdwl(ix,iy-4,ix+2,iy+4);
      drawdwl(ix+2,iy+4,ix+5,iy);
      drawdwl(ix+5,iy,ix+9,iy);  break;
    case 35 : 		/* flow bi-direc via closed left & right arrows */
      drawswl(ix-1,iy+7,ix+4,iy+5);
      drawswl(ix+4,iy+5,ix-1,iy+2);  /* from left */
      drawswl(ix-1,iy+2,ix-1,iy+7);
      drawswl(ix-6,iy+5,ix-1,iy+5);  /* shaft */
      drawswl(ix+1,iy-3,ix-4,iy-5);
      drawswl(ix-4,iy-5,ix+1,iy-8);  /* from right */
      drawswl(ix+1,iy-8,ix+1,iy-3);
      drawswl(ix+1,iy-5,ix+6,iy-5); break; /* shaft */
    case 36 :	       /* flow orifice 6*6 pixels double width */
      drawdwl(ix-4,iy-4,ix+3,iy-4);
      drawdwl(ix+3,iy-4,ix+3,iy+3);
      drawdwl(ix+3,iy+3,ix-4,iy+3);
      drawdwl(ix-4,iy+3,ix-4,iy-4);  break;
    case 37 :	       /* flow conduit */
      drawswl(ix-7,iy+5,ix,iy+8);
      drawswl(ix,iy+8,ix+5,iy+7);
      drawswl(ix+5,iy+7,ix+5,iy);
      drawswl(ix+5,iy,ix-3,iy-4);
      drawswl(ix-3,iy-4,ix-7,iy-2);
      drawswl(ix-7,iy-2,ix-7,iy+5);
      drawswl(ix-7,iy-2,ix,iy+3);
      drawswl(ix,iy+3,ix+5,iy);
      drawswl(ix,iy+3,ix,iy+8);
      drawswl(ix,iy+4,ix+5,iy+7);  break;
    case 38 :	       /* flow closed box */
      drawswl(ix+3,iy-2,ix+3,iy+5);
      drawswl(ix+3,iy+5,ix-2,iy+7);
      drawswl(ix-2,iy+7,ix-7,iy+3);
      drawswl(ix-7,iy+3,ix-7,iy-2);
      drawswl(ix-7,iy-2,ix-2,iy-4);
      drawswl(ix-2,iy-4,ix+3,iy-2);
      drawswl(ix-7,iy-2,ix-2,iy+1);
      drawswl(ix+3,iy-2,ix-2,iy+1);
      drawswl(ix-2,iy+7,ix-2,iy+1);  break;
    default : 		/* big dot 2 pix wide, 3 pix high */
      drawswl(ix-1,iy-1,ix+1,iy-1);
      drawswl(ix-1,iy,ix+1,iy);
      drawswl(ix-1,iy+1,ix+1,iy+1);  break;
  }
  return;
}

/* ***** drawpoint() draw a pixel point **** */
void drawpoint(xa,ya)
  gint xa,ya;
{
  gdk_draw_point(gr_image,gc,xa,ya);
}

/* ***** testcscale_() test drawing primitives & colors **** */
void testcscale_()
{
  PangoLayout *layout;
  PangoFontMetrics *metrics;
  gint ic,ih,ilw;
  GdkPoint points[10];
  char buffer[25];

/* should clear the bitmap first... << >> */
  if (gr_image) {
    g_object_unref(gr_image);
/* debug g_print("unreferenced pixmap\n");  */
  }
  gr_image = gdk_pixmap_new(graphic->window,
			    graphic->allocation.width,
			    graphic->allocation.height,
			    -1);

/* Clear pixmap by writing area in white then reset to black */
  gdk_gc_set_foreground(gc,&white);
  gdk_draw_rectangle (gr_image,gc,TRUE,0, 0,
	graphic->allocation.width,graphic->allocation.height);
  gdk_gc_set_foreground(gc,&black);

  ih=20;

/* Use Pango context setup in esp-r.c */
  metrics = pango_context_get_metrics (pango_context, graphic->style->font_desc,
	    pango_context_get_language (pango_context));
  f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
             pango_font_metrics_get_descent (metrics));
  f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (metrics));
  pango_font_metrics_unref (metrics);  /* metrics no longer needed */
/* debug   g_print("testcscale font height and width is %d %d\n", f_height,f_width); */
  layout = pango_layout_new (pango_context);

  for (ic=0; ic<45; ic++) {
    gdk_gc_set_foreground(gc,&cscale[ic]);
    gdk_draw_rectangle (gr_image,gc,FALSE,ih,ih, 20, 20);
    ih= ih + 6;
    if (ic <= 28 ) {
      gdk_gc_set_foreground(gc,&gscale[ic]);
      gdk_draw_rectangle (gr_image,gc,FALSE,ih+50,ih, 15, 20);
      gdk_gc_set_foreground(gc,&zscale[ic]);
      gdk_draw_rectangle (gr_image,gc,FALSE,ih+100,ih, 15, 15);
      buffer[0] = '\0';
      g_snprintf (buffer, sizeof (buffer), "%d", ic);
      pango_layout_set_text (layout, buffer, -1);
      gdk_draw_layout (gr_image, gc, ih+115, ih, layout);
    }
  }
  g_object_unref (layout);
  gdk_gc_set_foreground(gc,&gmenuhl);
  gdk_draw_rectangle (gr_image,gc,TRUE,200,80, 25, 25);
  buffer[0] = '\0';
  g_snprintf (buffer, sizeof (buffer), "%s", "rect 200 80 gmenuhl");
  layout = pango_layout_new (pango_context);
  pango_layout_set_text (layout, buffer, -1);
  gdk_draw_layout (gr_image, gc, ih+115, ih, layout);
  g_object_unref (layout);

  gdk_gc_set_foreground(gc,&gmodbg);
  gdk_draw_rectangle (gr_image,gc,TRUE,230,80, 25, 25);
  gdk_gc_set_foreground(gc,&gpopfr);
  gdk_draw_rectangle (gr_image,gc,TRUE,260,80, 25, 25);
  gdk_gc_set_foreground(gc,&gfeedfr);
  gdk_draw_rectangle (gr_image,gc,TRUE,290,80, 25, 25);
  gdk_gc_set_foreground(gc,&ginvert);
  gdk_draw_rectangle (gr_image,gc,TRUE,320,80, 25, 25);
  gdk_gc_set_foreground(gc,&grey50);
  gdk_draw_rectangle (gr_image,gc,TRUE,350,80, 25, 25);
  gdk_gc_set_foreground(gc,&grey43);
  gdk_draw_rectangle (gr_image,gc,TRUE,380,80, 25, 25);
  gdk_gc_set_foreground(gc,&infofg);
  gdk_gc_set_background(gc,&gmenuhl);
  gdk_draw_rectangle (gr_image,gc,FALSE,410,110, 25, 25);

  gdk_gc_set_line_attributes(gc,1,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,50,355,250,355);
  gdk_draw_line(gr_image,gc,300,355,350,305);

  gdk_gc_set_line_attributes(gc,2,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* double width */
  gdk_draw_line(gr_image,gc,50,360,250,360);
  gdk_draw_line(gr_image,gc,300,360,350,310);

  gdk_gc_set_line_attributes(gc,1,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);  /* even dash single width */
  gdk_draw_line(gr_image,gc,50,370,250,370);
  gdk_draw_line(gr_image,gc,300,370,350,320);

  gdk_gc_set_line_attributes(gc,2,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);  /* even dash double width */
  gdk_draw_line(gr_image,gc,50,375,250,375);
  gdk_draw_line(gr_image,gc,300,375,350,325);

  gdk_gc_set_background(gc,&grey43);  /* dash with background color for un-dash */
  gdk_gc_set_line_attributes(gc,2,GDK_LINE_DOUBLE_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_draw_line(gr_image,gc,50,380,250,380);
  gdk_draw_line(gr_image,gc,300,380,350,330);
  gdk_gc_set_background(gc,&white);

  ilw = 2;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_gc_set_dashes (gc, 0, "\2\4", 2);	/* dash style short solid with longer blank */
  gdk_draw_line(gr_image,gc,50,390,250,390);
  gdk_draw_line(gr_image,gc,300,390,350,340);
  gdk_gc_set_dashes (gc, 0, "\4\2", 2);	/* dash style long solid with shorter blank */
  gdk_draw_line(gr_image,gc,50,395,250,395);
  gdk_draw_line(gr_image,gc,300,395,350,345);
  gdk_gc_set_dashes (gc, 0, "\4\4", 2);	/* dash style even solid and blank seems equivalent to GDK_LINE_ON_OFF_DASH default*/
  gdk_draw_line(gr_image,gc,50,400,250,400);
  gdk_draw_line(gr_image,gc,300,400,350,350);

  ilw = 1;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_gc_set_dashes (gc, 0, "\2\4", 2);	/* dash style short solid with longer blank */
  gdk_draw_line(gr_image,gc,50,405,250,405);
  gdk_draw_line(gr_image,gc,300,405,350,355);
  gdk_gc_set_dashes (gc, 0, "\4\2", 2);	/* dash style long solid with shorter blank */
  gdk_draw_line(gr_image,gc,50,410,250,410);
  gdk_draw_line(gr_image,gc,300,410,350,360);
  gdk_gc_set_dashes (gc, 0, "\4\4", 2);	/* dash style even solid and blank seems equivalent to GDK_LINE_ON_OFF_DASH default*/
  gdk_draw_line(gr_image,gc,50,415,250,415);
  gdk_draw_line(gr_image,gc,300,415,350,365);
  gdk_gc_set_dashes (gc, 0, "\1\4", 2);	/* acts as dotted line, but not so good on slopes */
  gdk_draw_line(gr_image,gc,50,420,250,420);
  gdk_draw_line(gr_image,gc,300,420,350,370);
  gdk_gc_set_dashes (gc, 0, "\3\2\1\2", 4);	/* dash dot style ( 3 dash, 2 blank, point, 2 blank ) */
  gdk_draw_line(gr_image,gc,50,425,250,425);
  gdk_draw_line(gr_image,gc,300,425,350,375);

  ilw = 1;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* reset */
  gdk_draw_point(gr_image,gc,50,430);

  points[0].x = 100; points[0].y = 10;	/* draw a sequence of lines, single width */
  points[1].x = 109; points[1].y = 17;
  points[2].x = 103; points[2].y = 27;
  points[3].x = 100; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 3;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER);
  points[0].x = 130; points[0].y = 10;
  points[1].x = 139; points[1].y = 17;
  points[2].x = 133; points[2].y = 27;
  points[3].x = 130; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 3;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_ROUND,GDK_JOIN_ROUND);
  points[0].x = 150; points[0].y = 10;
  points[1].x = 159; points[1].y = 17;
  points[2].x = 153; points[2].y = 27;
  points[3].x = 150; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 2;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_gc_set_dashes (gc, 0, "\2\2", 2);	/* dash style short solid with longer blank */
  points[0].x = 170; points[0].y = 10;
  points[1].x = 179; points[1].y = 27;
  points[2].x = 173; points[2].y = 47;
  points[3].x = 170; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 1;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* reset */

/* debug g_print("about to draw pixmap on graphic\n");  */
  gdk_draw_drawable(graphic->window, gc, gr_image,0, 0, 0, 0,
			        graphic->allocation.width,
			        graphic->allocation.height);
/* debug  g_print("after draw_drawable call for graphic\n");  */
/*  g_object_unref (metrics); */
  return;
}

/* ***** testcscale_() test drawing primitives & colors **** */
void testcscalea_()
{
  PangoLayout *layout;
  PangoFontMetrics *metrics;
  gint ic,ih,ilw;
  GdkPoint points[10];
  char buffer[25];

/* should clear the bitmap first... << >> */
  if (gr_image) {
    g_object_unref(gr_image);
    /* fprintf(stderr,"unreferenced pixmap\n");  debug */
  }
  gr_image = gdk_pixmap_new(graphic->window,
			    graphic->allocation.width,
			    graphic->allocation.height,
			    -1);

/* Clear pixmap by writing area in white then reset to black */
  gdk_gc_set_foreground(gc,&white);
  gdk_draw_rectangle (gr_image,gc,TRUE,0, 0,
	graphic->allocation.width,graphic->allocation.height);
  gdk_gc_set_foreground(gc,&black);

  ih=20;

/* Use Pango context setup in esp-r.c */
  metrics = pango_context_get_metrics (pango_context, graphic->style->font_desc,
	    pango_context_get_language (pango_context));
  f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
             pango_font_metrics_get_descent (metrics));
  f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (metrics));
  pango_font_metrics_unref (metrics);	/* metrics no longer needed */
/* debug  g_print("testcscale font height and width is %d %d\n", f_height,f_width);  */
  layout = pango_layout_new (pango_context);

  for (ic=0; ic<45; ic++) {
    gdk_gc_set_foreground(gc,&cscale[ic]);
    gdk_draw_rectangle (gr_image,gc,FALSE,ih,ih, 20, 20);
    ih= ih + 6;
    if (ic <= 28 ) {
      gdk_gc_set_foreground(gc,&gscale[ic]);
      gdk_draw_rectangle (gr_image,gc,FALSE,ih+50,ih, 15, 20);
      gdk_gc_set_foreground(gc,&zscale[ic]);
      gdk_draw_rectangle (gr_image,gc,FALSE,ih+100,ih, 15, 15);
      buffer[0] = '\0';
      g_snprintf (buffer, sizeof (buffer), "%d", ic);
      pango_layout_set_text (layout, buffer, -1);
      gdk_draw_layout (gr_image, gc, ih+115, ih, layout);
    }
  }
  g_object_unref (layout);
  gdk_gc_set_foreground(gc,&gmenuhl);
  gdk_draw_rectangle (gr_image,gc,TRUE,200,80, 25, 25);
  g_snprintf (buffer, sizeof (buffer), "%s", "rect 200 80 gmenuhl");
  layout = pango_layout_new (pango_context);
  pango_layout_set_text (layout, buffer, -1);
  gdk_draw_layout (gr_image, gc, ih+115, ih, layout);
  g_object_unref (layout);

  gdk_gc_set_foreground(gc,&gmodbg);
  gdk_draw_rectangle (gr_image,gc,TRUE,230,80, 25, 25);
  gdk_gc_set_foreground(gc,&gpopfr);
  gdk_draw_rectangle (gr_image,gc,TRUE,260,80, 25, 25);
  gdk_gc_set_foreground(gc,&gfeedfr);
  gdk_draw_rectangle (gr_image,gc,TRUE,290,80, 25, 25);
  gdk_gc_set_foreground(gc,&ginvert);
  gdk_draw_rectangle (gr_image,gc,TRUE,320,80, 25, 25);
  gdk_gc_set_foreground(gc,&grey50);
  gdk_draw_rectangle (gr_image,gc,TRUE,350,80, 25, 25);
  gdk_gc_set_foreground(gc,&grey43);
  gdk_draw_rectangle (gr_image,gc,TRUE,380,80, 25, 25);
  gdk_gc_set_foreground(gc,&infofg);
  gdk_gc_set_background(gc,&gmenuhl);
  gdk_draw_rectangle (gr_image,gc,FALSE,410,110, 25, 25);

  ilw = 1;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* reset */

  /* fprintf(stderr,"at end of testcscalea_\n");  debug */
/*  g_object_unref (metrics); */
  return;
}

/* ***** testcscale_() test drawing primitives & colors **** */
void testcscaleb_()
{
  gint ih,ilw;
  GdkPoint points[10];

  ih=20;

  gdk_gc_set_line_attributes(gc,1,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,50,355,250,355);
  gdk_draw_line(gr_image,gc,300,355,350,305);

  gdk_gc_set_line_attributes(gc,2,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* double width */
  gdk_draw_line(gr_image,gc,50,360,250,360);
  gdk_draw_line(gr_image,gc,300,360,350,310);

  gdk_gc_set_line_attributes(gc,1,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);	/* even dash single width */
  gdk_draw_line(gr_image,gc,50,370,250,370);
  gdk_draw_line(gr_image,gc,300,370,350,320);

  gdk_gc_set_line_attributes(gc,2,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);	/* even dash double width */
  gdk_draw_line(gr_image,gc,50,375,250,375);
  gdk_draw_line(gr_image,gc,300,375,350,325);

  gdk_gc_set_background(gc,&grey43);	/* dash with background color for un-dash */
  gdk_gc_set_line_attributes(gc,2,GDK_LINE_DOUBLE_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_draw_line(gr_image,gc,50,380,250,380);
  gdk_draw_line(gr_image,gc,300,380,350,330);
  gdk_gc_set_background(gc,&white);

  ilw = 2;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_gc_set_dashes (gc, 0, "\2\4", 2);	/* dash style short solid with longer blank */
  gdk_draw_line(gr_image,gc,50,390,250,390);
  gdk_draw_line(gr_image,gc,300,390,350,340);
  gdk_gc_set_dashes (gc, 0, "\4\2", 2);	/* dash style long solid with shorter blank */
  gdk_draw_line(gr_image,gc,50,395,250,395);
  gdk_draw_line(gr_image,gc,300,395,350,345);
  gdk_gc_set_dashes (gc, 0, "\4\4", 2);	/* dash style even solid and blank seems equivalent to GDK_LINE_ON_OFF_DASH default*/
  gdk_draw_line(gr_image,gc,50,400,250,400);
  gdk_draw_line(gr_image,gc,300,400,350,350);

  ilw = 1;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_gc_set_dashes (gc, 0, "\2\4", 2);	/* dash style short solid with longer blank */
  gdk_draw_line(gr_image,gc,50,405,250,405);
  gdk_draw_line(gr_image,gc,300,405,350,355);
  gdk_gc_set_dashes (gc, 0, "\4\2", 2);	/* dash style long solid with shorter blank */
  gdk_draw_line(gr_image,gc,50,410,250,410);
  gdk_draw_line(gr_image,gc,300,410,350,360);
  gdk_gc_set_dashes (gc, 0, "\4\4", 2);	/* dash style even solid and blank seems equivalent to GDK_LINE_ON_OFF_DASH default*/
  gdk_draw_line(gr_image,gc,50,415,250,415);
  gdk_draw_line(gr_image,gc,300,415,350,365);
  gdk_gc_set_dashes (gc, 0, "\1\4", 2);	/* acts as dotted line, but not so good on slopes */
  gdk_draw_line(gr_image,gc,50,420,250,420);
  gdk_draw_line(gr_image,gc,300,420,350,370);
  gdk_gc_set_dashes (gc, 0, "\3\2\1\2", 4);	/* dash dot style ( 3 dash, 2 blank, point, 2 blank ) */
  gdk_draw_line(gr_image,gc,50,425,250,425);
  gdk_draw_line(gr_image,gc,300,425,350,375);

  ilw = 1;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* reset */
  gdk_draw_point(gr_image,gc,50,430);

  points[0].x = 100; points[0].y = 10;	/* draw a sequence of lines, single width */
  points[1].x = 109; points[1].y = 17;
  points[2].x = 103; points[2].y = 27;
  points[3].x = 100; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 3;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER);
  points[0].x = 130; points[0].y = 10;
  points[1].x = 139; points[1].y = 17;
  points[2].x = 133; points[2].y = 27;
  points[3].x = 130; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 3;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_ROUND,GDK_JOIN_ROUND);
  points[0].x = 150; points[0].y = 10;
  points[1].x = 159; points[1].y = 17;
  points[2].x = 153; points[2].y = 27;
  points[3].x = 150; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 2;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_gc_set_dashes (gc, 0, "\2\2", 2);	/* dash style short solid with longer blank */
  points[0].x = 170; points[0].y = 10;
  points[1].x = 179; points[1].y = 27;
  points[2].x = 173; points[2].y = 47;
  points[3].x = 170; points[3].y = 10;
  gdk_draw_lines(gr_image,gc,points,4);

  ilw = 1;
  gdk_gc_set_line_attributes(gc,ilw,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER); /* reset */

/* debug  g_print("about to draw pixmap on graphic\n");  */
  gdk_draw_drawable(graphic->window, gc, gr_image,0, 0, 0, 0,
			        graphic->allocation.width,
			        graphic->allocation.height);
/* debug  g_print("after draw_drawable call for graphic\n");  */
  return;
}


/* *************** elinewwc_() line drawing routine to file. *************** */
/*
 eline commands to file.
*/
void elinewwc_(long int* x,long int* y,long int* operation)
{

/* If echo send parameters to wwc file */
   if ( wwc_ok == 1) {
     fprintf(wwc,"*eline\n");
     fprintf(wwc,"%ld %ld %ld\n",*x,*y,*operation);
   }
   return;
}


/* *************** eline_() line drawing routine. *************** */
/*
 co-ords, operation flag equiv to fwwutil parameters:
	1=move to absolute pixel coord,
	0=draw line to absolute pixel coord,
	3=move to relitive pixel coord,
	2=draw line to relitive pixel coord.
*/
void eline_(long int* x,long int* y,long int* operation)
{
  gint x1,y1,op,width;

/* If echo send parameters to wwc file */
   if ( wwc_ok == 1) {
     fprintf(wwc,"*eline\n");
     fprintf(wwc,"%ld %ld %ld\n",*x,*y,*operation);
   }
   x1 = (gint) *x;
   y1 = (gint) *y;
   op = (gint) *operation;
   width = 1;
   gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */

   if      (op == 0) {    /* LNDRAWABS */
     gdk_draw_line(gr_image,gc,xold,yold,x1,y1);
     xold = x1; yold = y1;
   } else if (op == 1) {  /* LNMOVEABS */
     xold = x1; yold = y1;
   } else if (op == 2) {  /* LNDRAWREL */
     gdk_draw_line(gr_image,gc,xold,yold,xold+x1,yold+y1);
     xold = xold+x1; yold = yold+y1;
   } else if (op == 3) {  /* LNMOVEREL */
     xold = xold+x1; yold = yold+y1;
   }
   return;
}


/* *************** edlinewwc_() dotted line drawing to file. *************** */
/*
 edline commands to file.
*/
void edlinewwc_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edline\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }
  return;
}

/* *************** edline_() dotted line drawing routine. *************** */
/*
 This function is passed both sets of pixel co-ords.
 The ipdis is the number of pixels between each dot.  Typical values
 are 2 for a dense dotted line, 3 for a sparce one, values over 4 are
 probably not useful. If the line is short then plot only the initial
 point.
*/
void edline_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{
  gint ix1,iy1,ix2,iy2,ldis,ldash,width;
  double crow;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edline\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }

  ldash = (gint) *ipdis;
  ix1 = (gint) *x1;         /* first point */
  iy1 = (gint) *y1;
  ix2 = (gint) *x2;         /* end point  */
  iy2 = (gint) *y2;

/* Begin with finding out the length (pixels) of the line. */
  crow = (double) (( *x1 - *x2 ) * ( *x1 - *x2) + ( *y1 - *y2) * ( *y1 - *y2));
  if( crow <= 2.0) {
    gdk_draw_point(gr_image,gc,ix1,iy1);	/* if short line draw point */
    return;
  }
  ldis = (gint) sqrt(crow);
  if( ldis <= 3 ) {
    gdk_draw_point(gr_image,gc,ix1,iy1);	/* if short line draw point */
    return;
  }

  width = 1;	/* assume singe pixel wide line */
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER); /* typical settings */
  if( ldash == 1 ) {
    gdk_gc_set_dashes (gc, 0, "\1\1", 2);	/* dash style short solid with short blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 2 ) {
    gdk_gc_set_dashes (gc, 0, "\1\2", 2);	/* dash style short solid with 2 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 3 ) {
    gdk_gc_set_dashes (gc, 0, "\1\3", 2);	/* dash style short solid with 3 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 4 ) {
    gdk_gc_set_dashes (gc, 0, "\1\4", 2);	/* dash style short solid with 4 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 5 ) {
    gdk_gc_set_dashes (gc, 0, "\1\5", 2);	/* dash style short solid with 5 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash >= 6 ) {
    gdk_gc_set_dashes (gc, 0, "\2\6", 2);	/* dash style shortish solid with 6 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  }
  return;
}


/* *************** edashwwc_() dashed line drawing routine. *************** */
/*
 Edash commands to file.
*/
void edashwwc_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edash\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }
  return;
}

/* *************** edash_() dashed line drawing routine. *************** */
/*
 Edash draws a dashed line based on start and end point pixel co-ords.
 Ipdis is the number of pixels between each dot.  Typical values
 are 2 for a dense dashed line, 3 for a normal one, values over 4 are
 probably not useful. Two static values are used:
   dash_on : to say whether current increment is part of a dash,
   dash_rem: to give the pixel distance remainder.

 dash_on and dash_rem are cleared if edash call is made with ipdis = 0.

*/
void edash_(x1,y1,x2,y2,ipdis)
  long int *x1, *y1, *x2, *y2, *ipdis;
{
  gint ix1,iy1,ix2,iy2;
  gint ldash;
  gint width = 1;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edash\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }

  ldash = (gint) *ipdis;
  ix1 = (gint) *x1;         /* give values to local variables */
  iy1 = (gint) *y1;
  ix2 = (gint) *x2;
  iy2 = (gint) *y2;

/* NOTE: there is a possible gdk logic that can make use of dash_on and dash_rem - needs to be checked out */
  if(ldash == 0) {    /* clear dash_on & dash_rem and return */
    dash_on = 0;
    dash_rem = 0;
    return;
  }
  width = 1;	/* assume singe pixel wide line */
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER); /* typical settings */
  if( ldash == 1 ) {
    gdk_gc_set_dashes (gc, 0, "\2\1", 2);	/* dash style short solid with short blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 2 ) {
    gdk_gc_set_dashes (gc, 0, "\2\2", 2);	/* dash style short solid with 2 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 3 ) {
    gdk_gc_set_dashes (gc, 0, "\3\3", 2);	/* dash style short solid with 3 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 4 ) {
    gdk_gc_set_dashes (gc, 0, "\4\4", 2);	/* dash style short solid with 4 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 5 ) {
    gdk_gc_set_dashes (gc, 0, "\5\5", 2);	/* dash style short solid with 5 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash >= 6 ) {
    gdk_gc_set_dashes (gc, 0, "\6\6", 2);	/* dash style shortish solid with 6 pixel blank */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  }
  return;
}

/* *************** ESRU single width line to file. *************** */
/*
 Writes eswline attributest to file.
*/
void eswlinewwc_(x1,y1,x2,y2)
  long int *x1, *y1, *x2, *y2;
{
  if ( wwc_ok == 1 && wwc_macro != 1) {	/* If echo send parameters to wwc file */
    fprintf(wwc,"*eswline\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x1,*y1,*x2,*y2);
  }
  return;
}


/* *************** eswline_() single width line drawing routine. *************** */
/*
 Draws a single pixel wide line between two pixel coordinates passed from fortran.
*/
void eswline_(x1,y1,x2,y2)
  long int *x1, *y1, *x2, *y2;
{
  gint ix1,iy1,ix2,iy2,width;
  ix1 = (gint) *x1;         /* first point */
  iy1 = (gint) *y1;
  ix2 = (gint) *x2;         /* end point  */
  iy2 = (gint) *y2;
  width = 1;
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  return;
}


/* *************** edwlinewwc_() double width line drawing routine to file. *************** */
void edwlinewwc_(x1,y1,x2,y2)
  long int *x1, *y1, *x2, *y2;
{
/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edwline\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x1,*y1,*x2,*y2);
  }
  return;
}


/* *************** edwline_() double width line drawing routine. *************** */
/*
 Draws a two pixel wide line between two pixel coordinates.
*/
void edwline_(x1,y1,x2,y2)
  long int *x1, *y1, *x2, *y2;
{
  gint ix1,iy1,ix2,iy2,width;
  ix1 = (gint) *x1;         /* first point */
  iy1 = (gint) *y1;
  ix2 = (gint) *x2;         /* end point  */
  iy2 = (gint) *y2;
  width = 2;
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edwline\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x1,*y1,*x2,*y2);
  }
  return;
}


/* *************** echainwwc_() chained line drawing routine. *************** */
/*
 Echain commands to file.
*/
void echainwwc_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edash\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }
  return;
}

/* *************** echain_() chained line drawing routine. *************** */
/*
 Echain is passed start and end point pixel co-ords.
 The ipdis is the number of pixels between each dot.  Typical values
 are 3 for a dense chain, 4 for a normal one, values over 6 are
 probably not useful. Two static values are used:
   dash_on : to say whether current increment is part of a dash,
   dash_rem: to give the pixel distance remainder.

 dash_on and dash_rem are cleared if edash call is made with ipdis = 0.

*/
void echain_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{
  gint ix1,iy1,ix2,iy2;
  gint ldash;
  gint width = 1;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edash\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }

  ldash = (gint) *ipdis;
  ix1 = (gint) *x1;         /* give values to local variables */
  iy1 = (gint) *y1;
  ix2 = (gint) *x2;
  iy2 = (gint) *y2;

/* NOTE: there is a possible gdk logic that can make use of dash_on and dash_rem - needs to be checked out */
  if(ldash == 0) {    /* clear dash_on & dash_rem and return */
    dash_on = 0;
    dash_rem = 0;
    return;
  }
  width = 1;	/* assume singe pixel wide line */
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_ON_OFF_DASH,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* typical settings */
  if( ldash == 1 ) {
    gdk_gc_set_dashes (gc, 0, "\2\1\1\1", 4);	/* dash dot style ( 2 dash, 1 blank, point, 1 blank ) */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 2 ) {
    gdk_gc_set_dashes (gc, 0, "\2\2\1\2", 4);	/* dash dot style ( 2 dash, 2 blank, point, 2 blank ) */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 3 ) {
    gdk_gc_set_dashes (gc, 0, "\3\2\1\2", 4);	/* dash dot style ( 2 dash, 2 blank, point, 2 blank ) */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 4 ) {
    gdk_gc_set_dashes (gc, 0, "\4\2\2\2", 4);	/* dash dot style ( 4 dash, 2 blank, short dask, 2 blank ) */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash == 5 ) {
    gdk_gc_set_dashes (gc, 0, "\5\3\3\3", 4);	/* dash dot style ( 5 dash, 3 blank, 3 dask, 3 blank ) */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  } else if ( ldash >= 6 ) {
    gdk_gc_set_dashes (gc, 0, "\6\2\3\2", 4);	/* dash dot style ( 6 dash, 2 blank, 3 dask, 2 blank ) */
    gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  }
  return;
}

/* ***** drawdwl() draw a line of width 2 between two points **** */
void drawdwl(xa,ya,xb,yb)
  gint xa,ya,xb;
{
  gint width;
  width = 2;
  gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,xa,ya,xb,yb);
  return;
}

/* ***** drawvwl() draw a line of user defined width between two points **** */
void drawvwl(xa,ya,xb,yb,uwidth)
  gint xa,ya,xb,yb,uwidth;
{
  gdk_gc_set_line_attributes(gc,uwidth,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,xa,ya,xb,yb);
  return;
}

/* ***** drawddash() draw an double dashed line of user defined width between two points **** */
/* note: this leaves the gap with the underlying window colour (not necessarily the
 *       current bg colour */
void drawddash(xa,ya,xb,yb,uwidth)
  gint xa,ya,xb,yb,uwidth;
{
  gdk_gc_set_line_attributes(gc,uwidth,GDK_LINE_DOUBLE_DASH,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
  gdk_draw_line(gr_image,gc,xa,ya,xb,yb);
}

/* ***** drawoodash() draw an on-off dashed line of user defined width between two points **** */
void drawoodash(xa,ya,xb,yb,uwidth)
  gint xa,ya,xb,yb,uwidth;
{
  gdk_gc_set_line_attributes(gc,uwidth,GDK_LINE_ON_OFF_DASH,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_draw_line(gr_image,gc,xa,ya,xb,yb);
}


/* ************** axiscale_() determine scaling ratios ******************* */
/*
 Determine the scaling ratios based on the axis range of
 XMN to XMY for the horizontal axis and YMN to YMX for the vertical
 axis within a window "gw" pixels wide by "gh" pixels high. XSC is the
 scaling factor for the horizontal axis, YSC for the vertical and SCA
 is a scaling factor which gives an equal aspect ratio.  In mixed cases
 of minimum/maximum X/Y coords being negative set a data offset Xadd &
 Yadd to assist in line drawing ( window coords are 0,0 upper left
 whereas line drawing is 0,0 in the lower left).
*/

void axiscale_(long int* gw,long int* gh,float* xmn,float* xmx,float* ymn,
	float* ymx,float* xsc,float* ysc,float* sca,float* xadd,float* yadd)
{
  float axgw, axgh, axxmn, axxmx, axymn, axymx, axxsc, axysc, axsca, axxadd, axyadd;

/* Cast to local variables */
   axgw=(float)*gw; axgh=(float)*gh;
   axxmn=(float)*xmn; axxmx=(float)*xmx;
   axymn=(float)*ymn; axymx=(float)*ymx;
   axyadd=0.0;
   axxadd=0.0;
   axysc=0.0;
   axxsc=0.0;

/* Derive factors for horizontal axis. */
    if (axxmn < 0.0 && axxmx >= 0.0) {
	axxsc = axgw / (axxmx + (-1.0 * axxmn));
	axxadd = (-1.0 * axxmn);
    } else if (axxmn < 0.0 && axxmx <= 0.0) {
	axxsc = axgw / ((-1.0 * axxmn) - (-1.0 * axxmx));
	axxadd = (-1.0 * axxmn);
    } else if (axxmn > 0.0 && axxmx > 0.0) {
	axxsc = axgw / (axxmx - axxmn);
	axxadd = -(axxmn);
    } else if ((axxmn == 0.0) && axxmx > 0.0) {
	axxsc = axgw / (axxmx - axxmn);
	axxadd = 0.0;
    }
/* Derive factors for vertical axis. */
    if (axymn < 0.0 && axymx >= 0.0) {
	axysc = axgh / (axymx + (-1.0 * axymn));
	axyadd = (-1.0 * axymn);
    } else if (axymn < 0.0 && axymx <= 0.0) {
	axysc = axgh / ((-1.0 * axymn) - (-1.0 * axymx));
	axyadd = (-1.0 * axymn);
    } else if (axymn > 0.0 && axymx > 0.0) {
	axysc = axgh / (axymx - axymn);
	axyadd = -(axymn);
    } else if ((axymn == 0.0) && axymx > 0.0) {
	axysc = axgh / (axymx - axymn);
	axyadd = 0.0;
    }
    *xsc = axxsc;  /* cast from local to parameters */
    *ysc = axysc;
    *xadd = axxadd;
    *yadd = axyadd;
/*
 Choose single scale so will have correct aspect ratio for site plans etc.
*/
    *sca = axxsc;
    if (axysc < axxsc) {
	*sca = axysc;
    }

/* If echo send parameters to wwc file */
   if ( wwc_ok == 1) {
     fprintf(wwc,"*axiscale\n");
     fprintf(wwc,"%ld %ld %f %f %f %f %f %f %f %f %f \n",
	*gw,*gh,*xmn,*xmx,*ymn,*ymx,*xsc,*ysc,*sca,*xadd,*yadd);
    }
/*    fprintf(stderr,"axiscale %ld %ld %f %f %f %f %f %f %f %f %f \n",
	*gw,*gh,*xmn,*xmx,*ymn,*ymx,*xsc,*ysc,*sca,*xadd,*yadd);  */
  return;
} /* axscale_ */

/* ************** linescale_() store scaling parameters for lines *********** */
/*
 Store scaling parameters for lines so that calls to __ can be
 in user units.  The parameters are as follows and are derived
 from calles to win3d & axiscale:

   loff & boff are the pixel coords of the lower left corner of
     the drawing area is the axis origins.
   lscale & bscale are the scaling factors.
   ladd & badd are offsets in user units for each axis so that
     various data ranges can be acommodated (see axiscale).
*/
void linescale_(long int* loff,float* ladd,float* lscale,long int* boff,float* badd,
	float* bscale)
 {
/* static variables defined @ beginning of wwlib.c */
   x_off = (gint) *loff;
   x_add = *ladd;
   x_scale = *lscale;
   y_off = (gint) *boff;
   y_add = *badd;
   y_scale = *bscale;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1) {
    fprintf(wwc,"*linescale\n");
    fprintf(wwc,"%ld %f %f %ld %f %f\n",
		*loff,*ladd,*lscale,*boff,*badd,*bscale);
  }
  return;
}

/* *************** u2pixel_() user units to pixel coords. *************** */
/*
 Based on scaling data passed to linescale this returns the pixel
 co-ords for a particular sets of user data.
*/
void u2pixel_(float* ux,float* uy,long int* ix,long int* iy)
{
  float x,y;
  x = *ux;
  y = *uy;

  x=(x + x_add) * x_scale;
  y=(y + y_add) * y_scale;

  *ix = (long int) x_off + (long int) x;
  *iy = (long int) y_off - (long int) y;
  return;
}

/* *************** pixel2u_() pixel coords to user units  *************** */
/*
 Based on scaling data passed to linescale this returns the grid
 co-ords for a particular set of input pixcel co-ords.
*/
void pixel2u_(long int* ux,long int* uy,float* gx,float* gy)
{
  long int x,y;
  x = *ux;
  y = *uy;

  x = x -  x_off;
  y = y_off - y;

  *gx= (float) (x/x_scale) - (float) x_add;
  *gy= (float) (y/y_scale) - (float) y_add;

  return;
}

/* *************** etplotwwc_() general line plotting to wwc file. *************** */
/*
 As below, but ouputs lines to wwc file without drawing them.
*/
void etplotwwc_(ux,uy,updown,sym)
  float *ux, *uy;
  long int *updown, *sym;
{
  float x,y;
  gint isymbol,iupd,x1,x2,y1,y2,width;
  long int lx1,lx2,ly1,ly2,ipdis,isz,gs;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1) {
    fprintf(wwc,"*etplot\n");
    fprintf(wwc,"%f %f %ld %ld\n",*ux,*uy,*updown,*sym);
   }

   return;
 }

/* *************** etplot_() general line plotting. *************** */
/*
 Based on scaling data passed to linescale this draws a solid or
 dashed or dotted line or symbol as in tplot.
 Updown = 0 is pen up, > 0 pen down, -1 to -100 dashed line,
          -101 to -200 chain line, -201 to -300 dotted line,
 in each case the spacing of dashes etc is defined by the value
 ie. -4 is a dashed line 4 pixels long, -203 is dots @ 3 pixels.

 sym is the symbol number to plot, if 0 then no symbol, otherwise
 map tplot conventions to esymbol.

*/
void etplot_(ux,uy,updown,sym)
  float *ux, *uy;
  long int *updown, *sym;
{
  float x,y;
  gint isymbol,iupd,x1,x2,y1,y2,width;
  long int lx1,lx2,ly1,ly2,ipdis,isz,gs;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1) {
    fprintf(wwc,"*etplot\n");
    fprintf(wwc,"%f %f %ld %ld\n",*ux,*uy,*updown,*sym);
   }

  x = *ux;
  y = *uy;
  isymbol = (gint) *sym;
  iupd = (gint) *updown;

  x=(x + x_add) * x_scale;
  y=(y + y_add) * y_scale;

  x2 = x_off + (int) x;
  y2 = y_off - (int) y;

  x1 = xold;
  y1 = yold;

/* long ints for internal calls */
  lx1=x1; ly1=y1; lx2=x2; ly2=y2;
  wwc_macro = 1;    /* if echoing commands, turn off till end of etplot */
  if(iupd == 0) {
      xold = x2; yold = y2;      /* move to position */
  } else if (iupd > 0) {
      width = 1;
      gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
      gdk_draw_line(gr_image,gc,x1,y1,x2,y2);  /* solid line       */
  } else if ((iupd < 0 && iupd > -100)) {
      ipdis= abs(iupd);
      edash_(&lx1,&ly1,&lx2,&ly2,&ipdis);           /* dashed line      */
  } else if ((iupd < -101 && iupd > -200)) {
      ipdis= abs(iupd)-100;
      echain_(&lx1,&ly1,&lx2,&ly2,&ipdis);          /* chain line       */
  } else if ((iupd < -201 && iupd > -300)) {
      ipdis= abs(iupd)-200;
      edline_(&lx1,&ly1,&lx2,&ly2,&ipdis);          /* dotted line (or points) */
  } else if ((iupd < -301 && iupd > -400)) {
      edwline_(&lx1,&ly1,&lx2,&ly2);                /* thick line       */
  }

/* Symbol plotting */
  isz=0;
  switch ( isymbol ) {
    case 0:  gs=-1;  break;  /* nothing */
    case 1:  gs=9;   break;  /* x */
    case 2:  gs=13;  break;  /* small down arrow */
    case 3:  gs=12;  break;  /* small up arrow */
    case 4:  gs=0;   break;  /* square */
    case 5:  gs=1;   break;  /* small triangle */
    case 6:  gs=8;   break;  /* big dot */
    case 7:  gs=4;   break;  /* lw right triangle */
    case 8:  gs=5;   break;  /* lw left triangle */
    case 9:  gs=6;   break;  /* up left triangle */
    case 10: gs=7;   break;  /* up right triangle */
    case 11: gs=11;  break;  /* + */
    case 12: gs=14;  break;  /* > arrow */
    case 13: gs=15;  break;  /* < arrow */
    case 14: gs=3;   break;  /* diamond */
    case 15: gs=2;   break;  /* upside down triangle */
    case 16: gs=10;  break;  /* large * */
    case 17: gs=32;  break;  /* small dot */
    case 18: gs=16;  break;  /* dot with | */
    case 19: gs=17;  break;  /* dot with - */
    case 20: gs=18;  break;  /* solid triangle */
    case 21: gs=19;  break;  /* upside down solid triangle */
    case 22: gs=20;  break;  /* solid lower right triangle */
    case 23: gs=21;  break;  /* solid lower left triangle */
    case 24: gs=22;  break;  /* solid upper left triangle */
    case 25: gs=23;  break;  /* solid upper right triangle */
    case 26: gs=24;  break;  /* solid diamond */
    case 27: gs=26;  break;  /* diamond with | */
    case 28: gs=28;  break;  /* diamond with - */
    case 29: gs=30;  break;  /* solid left arrow */
    case 30: gs=31;  break;  /* solid right arrow */
    case 31: gs=32;  break;  /* solid 2x3 square */
    default: gs=-1;  break;  /* nothing */
  }
  if(gs >= 0)esymbol_(&lx2,&ly2,&gs,&isz);
  wwc_macro = 0;    /* if echoing commands, turn back on */

  xold = x2;     /* remember last point. */
  yold = y2;
  return;
}

/* *************** erectan_() rectangle drawing routine. *************** */
/*
 This draws a rectangle based on the size and position in user units
 and transforms into pixel positions via static values passed into
 the C code via a previous call to linescale.
*/
void erectan_(x,y,dx,dy,dt)
  float *x, *y,*dx,*dy,*dt;
{
  gfloat xo,yo,x1,y1,dx1,dy1,dt1,dd,xc,xs,yc,ys,ri;
  double xcd;
  gint   ix,iy,ix1,iy1,ix2,iy2,ix3,iy3;
  gint width = 1;
  long int   lix,liy;
  xo = *x;     /* origin x coord */
  yo = *y;     /* origin y coord */
  dx1 = *dx;   /* width (ie. x axis with no rotation) */
  dy1 = *dy;   /* height (ie. y axis with no rotation) */
  dt1 = *dt;   /* rotation in degrees */

  gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */

/* convert into pixels and move to origin*/
  lix = liy = 0;
  u2pixel_(&xo,&yo,&lix,&liy);
  ix= (gint) lix; iy= (gint) liy;

/* find transforms (converting degrees into radians) */
  ri = (gfloat) 3.14159/180.0;
  xcd = (double) (ri * dt1);
  dd = dy1/dx1;
  xc = dx1 * (gfloat) cos(xcd);
  xs = dx1 * (gfloat) sin(xcd);
  yc = dd * xc;
  ys = dd * xs;

  x1=xo+xc; y1=yo+xs;
  lix = liy = 0;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix1= (gint) lix; iy1= (gint) liy;
  gdk_draw_line(gr_image,gc,ix,iy,ix1,iy1);

  x1=x1-ys; y1=y1+yc;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix2= (gint) lix; iy2= (gint) liy;
  gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);

  x1=x1-xc; y1=y1-xs;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix3= (gint) lix; iy3= (gint) liy;
  gdk_draw_line(gr_image,gc,ix2,iy2,ix3,iy3);
  gdk_draw_line(gr_image,gc,ix3,iy3,ix,iy);  /* back to origin  */
  return;
}


/* *************** egrbox_() grey box drawing routine. *************** */
/*
 This draws a grey filled rectangle based on the size and position in user units
 and transforms into pixel positions via static values passed into
 the C code via a previous call to linescale. GTK note: gdk assumes a
 rectangle origin is the upper left corner while the calling code will
 be assuming origin is lower left corner. Adjustment required.
*/
void egrbox_(x,y,dx,dy,gp)
  float *x, *y,*dx,*dy;
  long int *gp;
{
  gint b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
  float xo,yo,x1,y1,dx1,dy1;
  gint   gpo,ix,iy,ix1,iy1,width,height;
  long int   lix,liy;
  xo = *x;     /* origin x coord */
  yo = *y;     /* origin y coord */
  dx1 = *dx;   /* delta width (ie. x axis) */
  dy1 = *dy;   /* delta height (ie. y axis) */
  gpo = (gint) *gp;   /* grey step as in gscale */

/* convert into pixels and move to origin*/
  lix = liy = 0;
  u2pixel_(&xo,&yo,&lix,&liy);
  ix= (gint) lix; iy= (gint) liy;

  x1 = xo + dx1; y1 = yo + dy1;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix1= (gint) lix; iy1= (gint) liy;

  width = ix1-ix;	/* width in pixels */
  height = iy-iy1;	/* height in pixels */

  b_bottom = iy; b_top = iy1;
  if((b_bottom-b_top)<=2)return; /* if no height return */
  b_left = ix; b_right = ix1;
  if((b_right-b_left)<=2)return; /* if no width return */

/*  g_print("use grey cscale %ld\n",ngscale); */

  if (gpo >= 0 && gpo < (gint) ngscale) {

/* draw grey filled box and then black unfilled box */
    gdk_gc_set_foreground(gc,&gscale[gpo]);
    gdk_gc_set_background(gc,&gscale[gpo]);
    gdk_draw_rectangle (gr_image,gc,TRUE,b_left,b_top, width, height);
    gdk_gc_set_foreground(gc,&black);
    gdk_draw_rectangle (gr_image,gc,FALSE,b_left,b_top, width, height);
  }else if(gpo >= 2 && gpo < 14) {
    gdk_gc_set_foreground(gc,&gmodbg);
    gdk_gc_set_background(gc,&gmodbg);
    gdk_draw_rectangle (gr_image,gc,TRUE,b_left,b_top, width, height);
    gdk_gc_set_foreground(gc,&black);
    gdk_draw_rectangle (gr_image,gc,FALSE,b_left,b_top, width, height);
  }else if(gpo >= 14 && gpo < 29) {
    gdk_gc_set_foreground(gc,&gfeedfr);
    gdk_gc_set_background(gc,&gfeedfr);
    gdk_draw_rectangle (gr_image,gc,TRUE,b_left,b_top, width, height);
    gdk_gc_set_foreground(gc,&black);
    gdk_draw_rectangle (gr_image,gc,FALSE,b_left,b_top, width, height);
  }else if(gpo >= 29) {
    gdk_gc_set_foreground(gc,&black);
    gdk_gc_set_background(gc,&black);
    gdk_draw_rectangle (gr_image,gc,TRUE,b_left,b_top, width, height);
  }
  gdk_gc_set_background(gc,&white);
  return;
}

/* *************** etriang_() triangle drawing routine. *************** */
/*
 This draws a triangle based on the size and position in user units
 and transforms into pixel positions via static values passed into
 the C code via a previous call to linescale.
       /|
      / |dy
     /__|
  x,y dx
*/
void etriang_(x,y,dx,dy,dt)
  float *x, *y,*dx,*dy,*dt;
{
  gfloat xo,yo,x1,y1,dx1,dy1,dt1,dd,xc,xs,yc,ys,ri;
  double xcd;
  gint   ix,iy,ix1,iy1,ix2,iy2;
  gint width = 1;
  long int   lix,liy;
  xo = *x;     /* origin x coord */
  yo = *y;     /* origin y coord */
  dx1 = *dx;   /* width (ie. x axis with no rotation) */
  dy1 = *dy;   /* height (ie. y axis with no rotation) */
  dt1 = *dt;   /* rotation in degrees */

  gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */

/* convert into pixels and move to origin*/
  lix = liy = 0;
  u2pixel_(&xo,&yo,&lix,&liy);
  ix= (gint) lix; iy= (gint) liy;

/* find transforms (converting degrees into radians) */
  ri = (gfloat) 3.14159/180.0;
  xcd = (double) (ri * dt1);
  xc = dx1 * (gfloat) cos(xcd);
  xs = dx1 * (gfloat) sin(xcd);
  dd = dy1/dx1;
  yc = dd * xc;
  ys = dd * xs;

  x1=xo+xc;  y1=yo+xs;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix1= (gint) lix; iy1= (gint) liy;
  gdk_draw_line(gr_image,gc,ix,iy,ix1,iy1);

  x1=x1-ys;  y1=y1+yc;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix2=(gint) lix; iy2= (gint) liy;
  gdk_draw_line(gr_image,gc,ix1,iy1,ix2,iy2);
  gdk_draw_line(gr_image,gc,ix2,iy2,ix,iy);  /* back to origin  */
  return;
}

/* *************** ecirc_() circle drawing routine. *************** */
/*
 co-ords, operation flag:
	0=draw circle is drawn with clear inside.
	1=draw filled black circle.
	2=to be added...
	3=to be added...
 passed the x,y centre and the radius, this is converted into position
 of the upper left corner of the bounding box (ul & ut) and the
 bounding box width and height is rad1 * 2.
*/
void ecirc_(x,y,rad,operation)
  long int *x, *y, *rad, *operation;
{
  gint x1,y1,rad1,op,ul,ut,boxdim;
  x1 = (gint) *x;
  y1 = (gint) *y;
  rad1 = (gint) *rad;
  op = (gint) *operation;
  ul = x1 - rad1; /* left edge */
  ut = y1 - rad1; /* top edge */
  boxdim = rad1 + rad1;

  if (op == 0) {
    gdk_draw_arc (gr_image,gc,
                FALSE,ul,ut,boxdim,boxdim,
                0,360 * 180);
  } else if (op == 1) {
    gdk_gc_set_foreground(gc,&black);
    gdk_gc_set_background(gc,&black);
    gdk_draw_arc (gr_image,gc,
                TRUE,ul,ut,boxdim,boxdim,
                0,360 * 180);
    gdk_gc_set_foreground(gc,&black);
    gdk_gc_set_background(gc,&white);
  }
  return;
}

/* *************** earc_() arc drawing routine. *************** */
/*
 co-ords, operation flag:
	0=draw arc is drawn with clear inside.
	1=draw filled black arc.
 passed the x,y centre (pixels) and the radius (pixels), and two
 angles, the first is degrees from 3-o'clock position (+ is
 counterclockwise) and the 2nd is degress from the first angle.
 See fig 6-1 in Xlib programming manual.
 These are converted into position of the upper left corner of the
 bounding box (ul & ut) and the bounding box width and height is rad1 * 2.
*/
void earc_(x,y,rad,ang1,ang2,operation)
  long int *x, *y, *rad, *operation, *ang1, *ang2;
{
  gint x1,y1,rad1,op,ul,ut,boxdim,an1,an2;
  x1 = (gint) *x;
  y1 = (gint) *y;
  rad1 = (gint) *rad;
  op = (gint) *operation;
  an1 = (gint) *ang1;
  an2 = (gint) *ang2;
  ul = x1 - rad1; /* left edge */
  ut = y1 - rad1; /* top edge */
  boxdim = rad1 + rad1;

  if (op == 0) {
    gdk_draw_arc (gr_image,gc,
                FALSE,ul,ut,boxdim,boxdim,an1*64, an2*64);
  } else if (op == 1) {
    gdk_gc_set_foreground(gc,&black);
    gdk_gc_set_background(gc,&black);
    gdk_draw_arc (gr_image,gc,
                TRUE,ul,ut,boxdim,boxdim,an1*64, an2*64);
    gdk_gc_set_foreground(gc,&black);
    gdk_gc_set_background(gc,&white);
  }
  return;
}

/* ************ labelstr() generate a tic label *************** */
/*
 Generate a tic label where n is the number of decimal places,
 val is the value to be converced, Wticc is the number of pixels
 required to center the text (horizontally), sstr is the string
 returned.
*/
void labelstr(n,val,WticC,sstr)
 long int *n;
 float *val;
 long int *WticC;
 char sstr[10];
{
  int idum, n1, ticc;
  float val1;
  gint f_width;	/* pixel width of default font */

  val1 = *val;
  n1 = (int) *n;
  ticc = 0;

/* Use Pango context previously setup in esp-r.c */
  f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (pango_metrics));

  if (n1==0){
    idum = (int) *val;
    sprintf(sstr,"%d",idum);
    ticc = ((int)strlen(sstr) * f_width)/2;
  } else if ( n1 == 1){
      sprintf(sstr,"%6.1f",val1);
      ticc= ((int)strlen(sstr) * f_width)/2;
  } else if ( n1 == 2){
      sprintf(sstr,"%6.2f",val1);
      ticc= ((int)strlen(sstr) * f_width)/2;
  } else {
      sprintf(sstr,"%6.1g",val1);
  }
  *WticC = ticc;
  return;
} /* labelstr */


/* **************  etlabelwwc_() display text as in old teklib tlabel to file ******* */
/*
 Version which writes to wwc file.
*/
void etlabelwwc_(msg,x,y,ipos,size,len)
  char  *msg;       /* character string  */
  int len;          /* length from f77   */
  long int *size;   /* font size */
  long int *ipos;   /* 0=centred, 1=right, 2=centred top,
                       3=left,4=centered bottom.
                    */
  float *x,*y;      /* position in user units */
{
  int t_len;	/* for the truncated length of msg */
  t_len = 0;
  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*etlabel\n");
    fprintf(wwc,"%f %f %ld %ld\n",*x,*y,*ipos,*size);
    fprintf(wwc,"%s\n",msg);
  }
  return;
} /* etlabelwwc */


/* **************  etlabel_() display text as in old teklib tlabel ******* */
/*
 Given a string 'msg' and a reference position x,y in user units
 where the string should be written based on static variables from
 previous call to linescale. GTK note: pango assumes upper left of
 the text so include adjustment when plotting.
*/
void etlabel_(msg,x,y,ipos,size,len)
  char  *msg;       /* character string  */
  int len;          /* length from f77   */
  long int *size;   /* font size */
  long int *ipos;   /* 0=centred, 1=right, 2=centred top,
                       3=left,4=centered bottom.
                    */
  float *x,*y;      /* position in user units */
{
  char buffer[248];
  float x1,y1;
  gint ix,iy,mid,rig,p2,p0;
  int t_len;	/* for the truncated length of msg */
  long int fsize;
  long int lix,liy;
  PangoLayout *layout;	/* pango layout for the text in the buffer */

  t_len = 0;
  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*etlabel\n");
    fprintf(wwc,"%f %f %ld %ld\n",*x,*y,*ipos,*size);
    fprintf(wwc,"%s\n",msg);
  }

/* Use Pango context previously setup in esp-r.c */
  f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics) +
            pango_font_metrics_get_descent (pango_metrics));
  f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (pango_metrics));
  layout = pango_layout_new (pango_context);  /* clear and set layout */
  buffer[0] = '\0';
  g_snprintf (buffer, sizeof (buffer), "%s", msg); /* copy msg into buffer */
/*  pango_layout_set_text (layout, buffer, -1);   add text */
  pango_layout_set_text (layout, buffer, t_len);  /* add t_len char of text */

  x1 = *x; y1 = *y; fsize = *size;

  lix = liy = 0;
  u2pixel_(&x1,&y1,&lix,&liy);  /* return pixel location of reference.   */
  ix = (gint) lix; iy = (gint) liy;           /* convert back to short int */

/* << it might be possible to use pango_align = PANGO_ALIGN_RIGHT or PANGO_ALIGN_LEFT or PANGO_ALIGN_CENTER
   << in a  pango_layout_set_alignment (display->layout, pango_align); call instead */
  rig = ix - (f_width * t_len);   /* right justified position */
  mid = ix - (f_width * t_len / 2); /* centred position */
  p2  = iy - f_height;                /* ref @ upper char */
  p0  = iy - (f_height / 2);          /* ref @ centred char */

  if (*ipos == 0) {
    gdk_draw_layout (gr_image, gc,mid,p0,layout);	/* draw it on the pixmap */
  } else if (*ipos == 1) {
    gdk_draw_layout (gr_image, gc,rig,p0,layout);
  } else if (*ipos == 2) {
    gdk_draw_layout (gr_image, gc,mid,p2,layout);
  } else if (*ipos == 3) {
    gdk_draw_layout (gr_image, gc,ix,p0,layout);
  } else if (*ipos == 4) {
    gdk_draw_layout (gr_image, gc,mid,iy,layout);
  }
  return;
} /* etlabel */


/* ************** vrtaxisddwwc_() construct and draw a vert axis to file*************** */
/*
 Version to write vert axis to file.
*/

void vrtaxisddwwc_(float* ymn,float* ymx,long int* offl,long int* offb,long int* offt,
	float* yadd,float* sca,long int* mode,float *dddy, long int *nny,long int* side,char* msg,int mlen)
{
/*
 Local variables: WticL is the maximum character width of a tic label,
 ix & iy are the pixel coords, vertadj is half of the text height.
*/
 gint ilen;
 char msg2[80];

 ilen = 0;
 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
    fprintf(wwc,"*vrtaxis\n");
    fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld %ld\n",
		*ymn,*ymx,*offl,*offb,*offt,*yadd,*sca,*mode,*side);
    fprintf(wwc,"%s\n",msg2);
 }
 return;
} /* vrtaxisddwwc */


/* ************** vrtaxisdd_() construct and draw a vert axis *************** */
/*
 Construct and draw a vertical axis via WW where: YMN,YMX are the data
 minimum & maximum values, offl & offb are the pixel coords of the
 lower start of the axis.  SCA is the scaling factor and Yadd
 is a data offset to adjust plotting for various data ranges.
 Mode = 1 for time axis, Mode = 0 for other data display types.
 Side = 0 lables and tic on left, Side = 1 labels and tic on right.
 msg is the axis label and mlen is it's length (passed from fortran).
 ddy is data interval, ny number of decimal places to use.
 TODO: pass in character offset for axis rather than assuming a
       fixed value.
*/

void vrtaxisdd_(float* ymn,float* ymx,long int* offl,long int* offb,long int* offt,
	float* yadd,float* sca,long int* mode,float *dddy, long int *nny,long int* side,char* msg,int mlen)
{
/*
 Local variables: WticL is the maximum character width of a tic label,
 ix & iy are the pixel coords, vertadj is half of the text height.
*/
 PangoLayout *layout;	/* pango layout for the text in the buffer */
 gint b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
 gint width;	/* width of line */
 gint s_0,s_1, s_2, s_3, s_4, s_5;
 gint ofl,ofb,oft,sid;
 char sstr[10], buf[2];
 gint l, n, ix, vertadj, iy, il,ilen, nintvl;
 gint iy1, ix1;
 gint last_label_pixel, label_width, mid;
 long int ny,wticc,mde;
 float yticv,ddy,rintvl,resid;
 char msg2[80];

/* Use Pango context previously setup in esp-r.c */
 f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics) +
            pango_font_metrics_get_descent (pango_metrics));
 f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (pango_metrics));
 layout = pango_layout_new (pango_context);	/* clear and set initail layout */

 ilen = 0;
 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
    fprintf(wwc,"*vrtaxis\n");
    fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld %ld\n",
		*ymn,*ymx,*offl,*offb,*offt,*yadd,*sca,*mode,*side);
    fprintf(wwc,"%s\n",msg2);
 }

 ofl = (gint) *offl; ofb = (gint) *offb; oft = (gint) *offt; mde = (gint) *mode; sid = (gint) *side;
 ny = (gint) *nny; ddy = *dddy;

/* set box extents of graphic for user later */
 b_top = 0; b_left = 0; b_right = graphic->allocation.width; b_bottom = graphic->allocation.height;

/* Find the maximum label text width.  */
 label_width = 0;
 yticv = *ymn;
 labelstr(&ny, &yticv, &wticc, sstr);
 label_width = (int) strlen(sstr);
 yticv = *ymx;
 labelstr(&ny, &yticv, &wticc, sstr);

/* Find the length of the axis label passed */
 if( (int) strlen(sstr) > label_width) label_width = (int) strlen(sstr);

/* Draw a scale vertical axis. */
 s_0 = ix1 = ofl;
 iy = iy1 = ofb;

/*
 If mode=1 and axis does not begin on an integer the location of
 the first tic needs to be adjusted.
*/
 yticv = *ymn;
 rintvl = (*ymx - *ymn) / ddy + 1.0;
 nintvl = (int) rintvl;
 width = 1;
 gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
 if (mde == 1) {
   resid = *ymn - (int) *ymn;
   if(*ymn < 0. && resid != 0.) {
       yticv = *ymn;
       iy = ofb - (int) (((float) yticv + *yadd) * *sca);
       gdk_draw_line(gr_image,gc,ofl,ofb,s_0,iy);
   } else if(*ymn > 0. && resid != 0.) {
       yticv = (*ymn + ddy);
       iy = ofb - (int) (((float) yticv + *yadd) * *sca);
       gdk_draw_line(gr_image,gc,ofl,ofb,s_0,iy);
       nintvl--;
   }
   iy1 = iy; ix1 = s_0;	/* remember position */
 }

/* Now put in the interum tic marks and labels. */
 vertadj = (gint)(f_height * 0.5);
 s_1 = nintvl;

/* Initial label position (at bottom of graphic area) to test against.   */
 last_label_pixel = graphic->allocation.height;

/* Loop from minimum to maximum by DDX incriments. */
 for (l = 1; l <= s_1; ++l) {

/* Based on the DINTT info generate the appropriate labels. */
   labelstr(&ny, &yticv, &wticc, sstr);
    if (sid == 0) {
      s_4 = ofl -5;
      s_2 = ofl -8;
    } else {
      s_4 = ofl +5;
      s_2 = ofl +8;
    }
    iy = ofb - (int) ((yticv + *yadd) * *sca);
    gdk_draw_line(gr_image,gc,ix1,iy1,s_0,iy);
    gdk_draw_line(gr_image,gc,s_0,iy,s_4,iy);
    iy1 = iy; ix1 = s_4;	/* remember position */

/*     s_3 = iy + vertadj;            font centered vertically     */
    s_3 = iy - vertadj;            /* font centered vertically     */
    if (sid == 0) {              /* position label left or right */
      s_5 = ofl - (((label_width+1) * f_width));
    } else {
      s_5 = ofl + f_width +2;
    }

    if (s_3 < (last_label_pixel - (f_height + vertadj))) {
      n = (int) strlen(sstr);
      pango_layout_set_text (layout, sstr, -1);	/* add text */
      gdk_draw_layout (gr_image, gc,s_5,s_3,layout);	/* draw it on the pixmap */
      last_label_pixel = s_3;
      gdk_draw_line(gr_image,gc,ix1,iy1,s_2,iy);	/* extra tic length at label */
    }
    ix = ix1 = ofl; iy1 = iy;	/* remember position */
    yticv += ddy;
  }

/* Finish off the rest of the axis if less than the full width  */
  ix = ofl;
  iy = ofb - (int) ((*ymx + *yadd) * *sca);
  gdk_draw_line(gr_image,gc,ix1,iy1,ix,iy);

/*
 Print out the axis label on left or right. Loop through each
 character in the string and placing in a buffer for printing.
 If label on right ensure a bit of space between characters and
 the right edge of box to allow for image capture. If on right
 offset by 3 characters.
*/
  if (sid == 0) {
      ix = b_left + (2 *f_width);
  } else {
      ix = b_right - (3 * f_width);
  }
  mid = oft + ((ofb - oft)/2);
  iy = mid - (vertadj * ilen);
  if ((ofb - oft) > (f_height * ilen)){
    for (l = 0; l < ilen; ++l) {
      buf[0] = msg[l]; buf[1] = '\0';
      pango_layout_set_text (layout, buf, -1);	/* add text */
      gdk_draw_layout (gr_image, gc,ix,iy,layout);	/* draw it on the pixmap */
      iy = iy + f_height;
    }
  }
  g_object_unref (layout);	/* clear the layout */
  return;
} /* vrtaxsdd_ */


/* ************ horaxisddwwc_() construct and draw a horiz axis to file *************** */
/*
 Write horizontal axis attributes to file.
*/

void horaxisddwwc_(float *xmn,float *xmx,long int *offl,long int *offr,long int *offb,
        float *xadd,float *sca,long int *mode,float *dddx, long int *nnx, char* msg,int mlen)
{
 gint ilen;
 char msg2[80];
 ilen = 0;
 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';

 if ( wwc_ok == 1) {
   fprintf(wwc,"*horaxis\n");
   fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode);
   fprintf(wwc,"%s\n",msg2);
 }
 return;
} /* horaxisddwwc_ */


/* ************ horaxisdd_() construct and draw a horiz axis *************** */
/*
 Construct and draw a horizontal axis via WW where: XMN,XMX are the data
 minimum & maximum values, offL & offB are the pixel coords of the
 left start of the axis.  SCA is the scaling factor and Xadd is a data
 offset to adjust plotting for various data ranges. mode defines how
 left starting point is adjusted. ddx is data interval, nx number
 of decimal places to use.
*/

void horaxisdd_(float *xmn,float *xmx,long int *offl,long int *offr,long int *offb,
        float *xadd,float *sca,long int *mode,float *dddx, long int *nnx, char* msg,int mlen)
{
/*
 Local variables:
 WticC is the pixel shift (horizontal) to centre the tic label, ix & iy
 are the pixel coords.
*/
 PangoLayout *layout;	/* pango layout for the text in the buffer */
 gint b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
 gint width;	/* width of line */
 gint s_1, s_2, s_3, s_4, s_5;
 gint ofl,ofb,ofr;
 char sstr[10];
 gint l, n, ix, iy, ix1, iy1, nintvl, ilen;
 gint last_label_right_pixel, label_width, mid;
 long int nx,wticc,mde;
 float ddx,xticv,rintvl,resid;
 char msg2[80];

/* Use Pango context previously setup in esp-r.c */
 f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics) +
            pango_font_metrics_get_descent (pango_metrics));
 f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (pango_metrics));
 layout = pango_layout_new (pango_context);	/* clear and set layout */

 ilen = 0;
 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';

 if ( wwc_ok == 1) {
   fprintf(wwc,"*horaxis\n");
   fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode);
   fprintf(wwc,"%s\n",msg2);
 }
/* debug   fprintf(stderr,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode); */

 ofl = (gint) *offl; ofr = (gint) *offr; ofb = (gint) *offb; mde = *mode;
 nx = (gint) *nnx; ddx = *dddx;

/* set box extents of graphic for user later */
 b_top = 0; b_left = 0; b_right = graphic->allocation.width; b_bottom = graphic->allocation.height;

/* Find the maximum label text width.  */
 label_width = 0;
 xticv = *xmn;
 labelstr(&nx, &xticv, &wticc, sstr);
 label_width = (int) strlen(sstr);
 xticv = *xmx;
 labelstr(&nx, &xticv, &wticc, sstr);
 if( (int) strlen(sstr) > label_width) label_width = (int) strlen(sstr);

/* Draw a horizontal axis. */
 ix = ix1 = ofl;
 iy = iy1 = ofb;

/*
 If mode=1 and axis does not begin on an integer the location of
 the first tic needs to be adjusted.
*/
 xticv = *xmn;
 rintvl = (*xmx - *xmn) / ddx + 1.0;
 nintvl = (gint) rintvl;
 width = 1;
 gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
 if (mde == 1) {
   resid = *xmn - (int) *xmn;
   if(*xmn < 0. && fabs(resid) > 0.0001) { /* ?? fabs((double)resid) */
       xticv = *xmn;
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       gdk_draw_line(gr_image,gc,ofl,ofb,ix,iy);
   } else if(*xmn > 0. && fabs(resid) > 0.0001) { /* ?? fabs((double)resid) */
       xticv = (*xmn + ddx);
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       gdk_draw_line(gr_image,gc,ofl,ofb,ix,iy);
       nintvl--;     /* adjust to account for shift */
   }
   iy1 = iy; ix1 = ix;	/* remember position */
 }

/* Loop from minimum to maximum by DDX incriments. */

 s_1 = nintvl;
 last_label_right_pixel = 0;
 for (l = 1; l <= s_1; ++l) {

/* Based on the DINTT info generate the appropriate tic label "sstr". */
   labelstr(&nx, &xticv, &wticc, sstr);
   ix = ofl + (int) ((xticv + *xadd) * *sca);
   iy = ofb;
   gdk_draw_line(gr_image,gc,ix1,iy1,ix,iy);
   s_4 = iy + 5;                    /* tic descender position  */
   gdk_draw_line(gr_image,gc,ix,iy,ix,s_4);
   s_2 = (gint) (ix - wticc);                /* current label position  */
   s_5 = ix - ((label_width * f_width)/2); /* position if all label characters */
/*    s_3 = iy +f_height + 5; bottom of font  */
   s_3 = iy + 7; /* tick characters position (at top of font) below descender  */
   n = (int) strlen(sstr);
   if (s_5 >= (last_label_right_pixel + f_width)) {
     pango_layout_set_text (layout, sstr, -1);	/* add text */
     gdk_draw_layout (gr_image, gc,s_2,s_3,layout);	/* draw it on the pixmap */
     last_label_right_pixel = s_5 + (label_width * f_width);
     gdk_draw_line(gr_image,gc,ix,iy,ix,s_4+2);  /* extra tic length at label */
   }
   ix = ix1 = ofl; iy1 = iy;	/* remember position */
   xticv += ddx;
 }
/* Finish off the rest of the axis if less than the full width  */
 ix = ofl + (int) ((*xmx + *xadd) * *sca);
 iy = ofb;
 gdk_draw_line(gr_image,gc,ix1,iy1,ix,iy);

/* Print out the axis label. Note: gdk_draw_layout coordinates seem to
   be at the top of the font. */
 iy = b_bottom- (10 +f_height);
 mid = ofl + ((ofr - ofl)/2);
 ix = mid - (f_width * ilen /2);
 if (ix > 5){
   pango_layout_set_text (layout, msg2, -1);	/* add text */
   gdk_draw_layout (gr_image, gc,ix,iy,layout);	/* draw it on the pixmap */
 }
 g_object_unref (layout);	/* clear the layout */
 return;
} /* horaxisdd_ */


/* ************ horaxishdwdd_() construct and draw a horiz axis *************** */
/*
 Construct and draw a horizontal axis via WW where: XMN,XMX are the data
 minimum & maximum values, offL & offB are the pixel coords of the
 left start of the axis.  SCA is the scaling factor and Xadd is a data
 offset to adjust plotting for various data ranges. mode defines how
 left starting point is adjusted.
*/

void horaxishdwdd_(float *xmn,float *xmx,long int *offl,long int *offr,long int *offb,
        float *xadd,float *sca,long int *mode,float *dddx, long int *nnx,long int *ind,
	long int *idiv,long int *isjday,char* msg,int mlen)
{
/*
 Local variables:
 WticC is the pixel shift (horizontal) to centre the tic label, ix & iy
 are the pixel coords.
*/
 PangoLayout *layout;	/* pango layout for the text in the buffer */
 gint f_height;	/* pixel height of default font */
 gint f_width;	/* pixel width of default font */
 gint b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
 gint width;	/* width of line */
 gint s_1, s_2, s_3, s_4, s_5;
 gint ofl,ofb,ofr;
 char sstr[10];
 gint l, n, ix, iy, ix1, iy1, nintvl, ilen, iind, iidiv, iisjday;
 gint last_label_right_pixel, label_width, mid;
 long int nx,wticc,mde;
 float xticv,xxticv,ddx,rintvl,resid;
 char msg2[80];

/* Use Pango context previously setup in esp-r.c */
 f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics) +
            pango_font_metrics_get_descent (pango_metrics));
 f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (pango_metrics));
 layout = pango_layout_new (pango_context);	/* clear and set layout */

 ilen = 0;
 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';

 if ( wwc_ok == 1) {
   fprintf(wwc,"*horaxishdw\n");
   fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode);
   fprintf(wwc,"%s\n",msg2);
 }
/* debug   fprintf(stderr,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode); */

 ofl = (gint) *offl; ofr = (gint) *offr; ofb = (gint) *offb; mde = *mode;
 nx = (gint) *nnx; ddx = *dddx;
 iind = (gint) *ind; iidiv = (gint) *idiv; iisjday = (gint) *isjday;

/* set box extents of graphic for user later */
 b_top = 0; b_left = 0; b_right = graphic->allocation.width; b_bottom = graphic->allocation.height;

/* Find the maximum label text width based on which ind.  */
 label_width = 0;
 if(iind == 0) {
   xticv = *xmn/(float)iidiv;
 } else if (iind == 1) {
   xticv = *xmn/(float)iidiv;
 } else if (iind == 2) {
   xticv = *xmn/(float)iidiv;
 } else if (iind == 3) {
   xticv = *xmn/(float)iidiv;
 } else if (iind == 4) {
   xticv = *xmn;
 }
 labelstr(&nx, &xticv, &wticc, sstr);
 label_width = (int) strlen(sstr);
 if(iind == 0) {
   xticv = *xmx/(float)iidiv;
 } else if (iind == 1) {
   xticv = *xmx/(float)iidiv;
 } else if (iind == 2) {
   xticv = *xmx/(float)iidiv;
 } else if (iind == 3) {
   xticv = *xmx/(float)iidiv;
 } else if (iind == 4) {
   xticv = *xmx;
 }
 labelstr(&nx, &xticv, &wticc, sstr);
 if( (int) strlen(sstr) > label_width) label_width = (int) strlen(sstr);

/* Draw a horizontal axis. */
 ix = ix1 = ofl;
 iy = iy1 = ofb;

/*
 If mode=1 and axis does not begin on an integer the location of
 the first tic needs to be adjusted.
*/
 xticv = *xmn;
 rintvl = (*xmx - *xmn) / ddx + 1.0;
 nintvl = (gint) rintvl;
 width = 1;
 gdk_gc_set_line_attributes(gc,width,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER); /* gives same as default */
 if (mde == 1) {
   resid = *xmn - (int) *xmn;
   if(*xmn < 0. && fabs(resid) > 0.0001) {  /* ?? fabs((double)resid) */
       xticv = *xmn;
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       gdk_draw_line(gr_image,gc,ofl,ofb,ix,iy);
   } else if(*xmn > 0. && fabs(resid) > 0.0001) {  /* ?? fabs((double)resid) */
       xticv = (*xmn + ddx);
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       gdk_draw_line(gr_image,gc,ofl,ofb,ix,iy);
       nintvl--;     /* adjust to account for shift */
   }
   iy1 = iy; ix1 = ix;	/* remember position */
 }

/* Loop from minimum to maximum by DDX incriments. */

 s_1 = nintvl;
 last_label_right_pixel = 0;
 for (l = 1; l <= s_1; ++l) {

/* Based on the DINTT info generate the appropriate tic label "sstr". */
   if(iind == 0) {
     xxticv = xticv/(float)iidiv;
   } else if (iind == 1) {
     xxticv = xticv/(float)iidiv;
   } else if (iind == 2) {
     xxticv = xticv/(float)iidiv;
   } else if (iind == 3) {
     xxticv = xticv/(float)iidiv;
   } else if (iind == 4) {
     xxticv = xticv;
   }
   labelstr(&nx, &xxticv, &wticc, sstr);
   ix = ofl + (int) ((xticv + *xadd) * *sca);
   iy = ofb;
   gdk_draw_line(gr_image,gc,ix1,iy1,ix,iy);
   s_4 = iy + 5;                    /* tic descender position  */
   gdk_draw_line(gr_image,gc,ix,iy,ix,s_4);
   s_2 = (gint) (ix - wticc);                /* current label position  */
   s_5 = ix - ((label_width * f_width)/2); /* position if all label characters */
/*    s_3 = iy +f_height + 5; bottom of font  */
   s_3 = iy + 7; /* tick characters position (at top of font) below descender  */
   n = (int) strlen(sstr);
   if (s_5 >= (last_label_right_pixel + f_width)) {
     pango_layout_set_text (layout, sstr, -1);	/* add text */
     gdk_draw_layout (gr_image, gc,s_2,s_3,layout);	/* draw it on the pixmap */
     last_label_right_pixel = s_5 + (label_width * f_width);
     gdk_draw_line(gr_image,gc,ix,iy,ix,s_4+2);  /* extra tic length at label */
   }
   ix = ix1 = ofl; iy1 = iy;	/* remember position */
   xticv += ddx;
 }
/* Finish off the rest of the axis if less than the full width  */
 ix = ofl + (int) ((*xmx + *xadd) * *sca);
 iy = ofb;
 gdk_draw_line(gr_image,gc,ix1,iy1,ix,iy);

/* Print out the axis label. Note: gdk_draw_layout coordinates seem to
   be at the top of the font. */
 iy = b_bottom- (10 +f_height);
 mid = ofl + ((ofr - ofl)/2);
 ix = mid - (f_width * ilen /2);
 if (ix > 5){
   pango_layout_set_text (layout, msg2, -1);	/* add text */
   gdk_draw_layout (gr_image, gc,ix,iy,layout);	/* draw it on the pixmap */
 }
 g_object_unref (layout);	/* clear the layout */
 return;
} /* horaxishdwdd_ */


/* ***** popupimage_() display image with documentation */
/* Could pass in:
    title for the pop up box
*/
void popupimage_(char *prom,char *docu,char *act,char *file,int lenprom,int lendocu,int lenact,int lenfile)
/* prom is the prompt for the image
   docu is the documentation for the image (which might be editable)
   file is the name of the image file
   act = '-' documentation not to be edited, act = 'e' documentation can be edited (not implemented)
*/
{
   PangoFontDescription *pfd;	/* to hold test font */
   PangoContext *context;	/* for use in finding font properties */
   static GtkWidget *askbox;
   GtkWidget *entry, *label;
   GtkWidget *image;	/* space to draw the image into */
   GtkWidget *closebutton;	/* button to close the popup */
   GdkPixbuf *loadgpbimage;	/* the buffer for the image loaded from *file */
   gint XSize, YSize;	/* size of the image */
   gint imageloaded;	/* imageloaded = 0 false, imageloaded = 1 true */
   gchar *docu_local;
   gchar *question_local;
   gchar *file_local;		/* local string variable for image file name */
   int lnprom, lnfile, lndoc;	/* for non-blank length */

   f_to_c_l(docu,&lendocu,&lndoc);  /* find actual length of the documentation string. */
   lndoc = lndoc + 1;  /* add a bit of cushion */
   docu_local = g_strndup(docu, (gsize) lndoc);	/* make local string for documentation */

   lnprom = 0;  /* find out actual length of prompt */
   f_to_c_l(prom,&lenprom,&lnprom);
   question_local = g_strndup(prom, (gsize) lnprom);

   lnfile = 0;  /* find out actual length of the image file name */
   f_to_c_l(file,&lenfile,&lnfile);
   file_local = g_strndup(file, (gsize) lnfile);

/* debug g_print("phrase %s\n",question_local);  */
/* debug g_print("documentation is %s\n",docu_local); */
/* debug g_print("image is %s\n",file_local); */
/* debug g_print("nb of help lines %d\n",help_lines); */

   /* Create the widgets */
   askbox = gtk_dialog_new();	/* create a new dialog (pop-up box) */
   gtk_container_set_border_width (GTK_CONTAINER (askbox), 5);
   gtk_box_set_spacing((GtkBox *) GTK_DIALOG(askbox)->vbox, 2);
   gtk_window_set_title (GTK_WINDOW (askbox), question_local);			/* Set window title */

   loadgpbimage = gdk_pixbuf_new_from_file(file_local,NULL);	/* Load the file into a pixbuf */
   if( loadgpbimage==NULL) {
     g_print("error loading image file %s\n",file_local);	/* if there is a problem set small size */
     XSize = 150;
     YSize = 100;
     imageloaded = 0;
   } else {
     XSize = gdk_pixbuf_get_width(loadgpbimage);	/* get the actual size of the pixbuf */
     YSize = gdk_pixbuf_get_height(loadgpbimage);
     imageloaded = 1;
     /* debug g_print("loaded image file %s %d %d\n",file_local,XSize,YSize); */
   }
   image = gtk_image_new_from_pixbuf(loadgpbimage);	/* create a gtk image from the pixbuf */
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),image);
   gtk_widget_set_size_request ( image, XSize+5,YSize+5);

   entry = gtk_text_view_new ();  /* create a multi-line text editing area */
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),entry);

/* now set the font size to be used for the image documentation. */
   if (disp_fnt == 4 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     f_height = font_calculations_array[serif_small].f_height;
     f_width  = font_calculations_array[serif_small].f_width;
/*    g_print("configure font medium 8\n"); debug */
   } else if (disp_fnt == 5 ) {
     pfd = pango_font_description_from_string("Serif,Medium 9");
     f_height = font_calculations_array[serif_medium].f_height;
     f_width  = font_calculations_array[serif_medium].f_width;
/*    g_print("configure font medium 9\n"); debug */
   } else if (disp_fnt == 6 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     f_height = font_calculations_array[serif_large].f_height;
     f_width  = font_calculations_array[serif_large].f_width;
/*    g_print("configure font medium 10\n"); debug */
   } else if (disp_fnt == 7 ) {
     pfd = pango_font_description_from_string("Serif,Medium 11");
     f_height = font_calculations_array[serif_largest].f_height;
     f_width  = font_calculations_array[serif_largest].f_width;
/*    g_print("configure font medium 11\n"); debug */
   } else if (disp_fnt == 0 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 8");
     f_height = font_calculations_array[courier_small].f_height;
     f_width  = font_calculations_array[courier_small].f_width;
/*    g_print("configure Mono medium 8\n"); debug */
   } else if (disp_fnt == 1 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 9");
     f_height = font_calculations_array[courier_medium].f_height;
     f_width  = font_calculations_array[courier_medium].f_width;
/*    g_proint("configure Mono medium 9\n");  debug */
   } else if (disp_fnt == 2 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 10");
     f_height = font_calculations_array[courier_large].f_height;
     f_width  = font_calculations_array[courier_large].f_width;
/*    g_print("configure Mono medium 10\n");  debug */
   } else if (disp_fnt == 3 ) {
     pfd = pango_font_description_from_string("Monospace,Medium 11");
     f_height = font_calculations_array[courier_largest].f_height;
     f_width  = font_calculations_array[courier_largest].f_width;
/*    g_print("configure Mono medium 11\n");  debug */
   }
   gtk_widget_modify_font(entry, pfd);
   pango_font_description_free(pfd);

   gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (entry)),
			    docu_local, -1);	/* put docu_local text in the entry text display */

   gtk_text_view_set_left_margin (GTK_TEXT_VIEW (entry), 8);	/* provide left and right gap */
   gtk_text_view_set_right_margin (GTK_TEXT_VIEW (entry), 8);

   gtk_text_view_set_wrap_mode ( GTK_TEXT_VIEW (entry), GTK_WRAP_WORD );       /* warp the words */
   if ( XSize >= 250 ) {
     gtk_widget_set_size_request ( entry, XSize,-1);	/* width same as image with auto height  */
   } else {
     gtk_widget_set_size_request ( entry, XSize,-1);	/* width same as image with auto height */
   }

/* Make a button for closing the popup and display the popup. Use the signal to destroy the widget. */
   closebutton = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->action_area), closebutton);
   gtk_widget_set_size_request ( closebutton, 80,-1);
   g_signal_connect_swapped (GTK_OBJECT (closebutton),"clicked",
                             G_CALLBACK (gtk_widget_destroy),GTK_OBJECT (askbox));

   gtk_widget_show_all (askbox);

   return;

}
