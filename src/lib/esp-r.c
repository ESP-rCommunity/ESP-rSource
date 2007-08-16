/* Functions to set up the main display window and
   divide it into frames: graphics, text, menu dialog.

   Functions:
     destroy       - kill the program
     winfin_       - kill the program (called from fortran code)
     create_text   - create text feedback area and set display policies
     create_static_menus - create static menus associated with the interface
     create_dialog - create dialog (usrmsg) area and set display policies
     createwin    - create basic window arrangement
     configure_event - handles ??
     expose_event    - handles exposure of drawable area

     winclr	   - clears whole application to white
     win3d()       - finds pixel offsets within graphics window
     startbuffer_() clear view/3d display box.
     forceflush_() force graphic buffer to flush
*/

#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <glib.h>
#include <esp-r.h>
#include <commons.h>

#define ESP_LIST_MAIN
#include <esp_list.h>
#undef ESP_LIST_MAIN


/* Global variable definitions start here */
FILE *wwc;
GdkColor cscale[49], zscale[100], gscale[49];
long int ncscale;   /* number of assigned colours in colour scale */
long int ngscale;   /* number of assigned colours in grey scale */
long int ngr;       /* number of assigned interface colours */
long int izc;       /* number of assigned zone colours */
long int mdepth;    /* to pass to fortran: sceen depth */
long int disp_fnt;  /* font for text display box */
long int butn_fnt;  /* button font size     */
long int menu_fnt;  /* preferred command menu font */

GdkColor fg, bg, bd, bw, white, black, infofg, infobg;	/* essential colors */
GdkColor gmenuhl, gpopfr, gfeedfr, ginvert, gmodbg, grey50, grey43;	/* interface colors */

gint ter;            /* terminal type passed on initial call */
gint child_ter;      /* child process terminal type  */

gint  wwc_ok;   /* assume no echo of drawing commands to wwc */
gint  wwc_macro;   /* assume not in a macro drawing command (etplot) */

gint xold, yold;     /* current 'pen' position used by eline_() */
gint x_off,y_off;         /* display X & Y offsets in pixels (see linescale) */
gfloat x_add,x_scale,y_add,y_scale;  /* X & Y axis scales and data offsets (see linescale) */
gint dash_on;         /* toggle for dashed section of a line in edash_() */
gint dash_rem;        /* remaining pixel length in incomplete dash in edash_() */
gint dbx1_avail;      /* flag for existance of graphic display box */
gint c3dcl,c3dcr,c3dct,c3dcb; /* dbx1 char offsets left, right, top, bottom */
int help_width;	/* current help max line length */
int help_lines;	/* current number of active help lines */
int wire_avail;      /* wireframe adjustment holding nb of zones to draw */
char pm_list[MENU_LIST_LEN][125];	/* character arrays for menu buffer */
char pmtype_list[MENU_LIST_LEN];	/* character array representing m_list array use */
int pm_width;		/* current popup menu max line length */
int pm_lines;		/* current number of active popup menu lines */

/* Global variables for current font width and height. */
gint f_height;
gint f_width;

char cappl[5];	/* f77 application name */
char cfgroot[25];	/* f77 project root name    */
char path[73];	/* f77 project path    */
char upath[73];	/* f77 users path    */
char imgpth[25];	/* f77 relative path to images    */
char docpth[25];	/* f77 relative path to documents    */
char tmppth[25];	/* f77 relative path to scratch folder    */
char capt_wf_exe[73];	/* command to execute for capture wire frame */
char capt_tf_file[73];	/* file for text feedback buffer dump */
char capt_all_exe[73];	/* command for capture all of display */
char captut_exe[73];	/* command for tutorial */
int browse;	/* if = 0 then user owns, if = 1 user browsing */
gint xrt_width, xrt_height;  /* same as xsh.width and xsh.height */

gint menu_pix_wd;	/* pixel width of initial menu (based on nb of characters *imenuchw) */
long int menuchw;	/* char width of initial menu (from fortran) */

char zonenames[MCOM][13];  /* g pointer array to current zone names - filled by calling the fortran routine getzonenames() */



/* Function calculate_font_metrics  pre-processes font metrics */ 
void calculate_font_metrics(void);

/* The following code calculates font calculation metrics at the beginning of the program
 * When the window is iniltialized for the first time, all the calculations are done.
 * These resuts are then assigned to the local variables at runtime.
 */

void calculate_font_metrics(void){
  PangoFontDescription *pfd;	 /* to hold test font  */
  PangoContext *context;	 /* for use in finding font properties */ 
  PangoFontMetrics *metrics;
  long int cur_disp_fnt;  /* initial font for text display box */

  cur_disp_fnt = disp_fnt;  /* remember initial value */

  for( disp_fnt = 0; disp_fnt <=7; disp_fnt++){             // this sets the font to Serif Small
   
    if (disp_fnt == 0)     
      pfd = pango_font_description_from_string("Serif,Medium 8"); 
    else if (disp_fnt == 1)
      pfd = pango_font_description_from_string("Serif,Medium 10");
    else if(disp_fnt == 2)
      pfd = pango_font_description_from_string("Serif,Medium 12");
    else if(disp_fnt ==3)
      pfd = pango_font_description_from_string("Serif,Medium 14");
    else if(disp_fnt ==4)
      pfd = pango_font_description_from_string("Courier,Medium 8");
    else if(disp_fnt == 5)
      pfd = pango_font_description_from_string("Courier,Medium 10");
    else if(disp_fnt == 6)
      pfd = pango_font_description_from_string("Courier,Medium 12");
    else if(disp_fnt == 7)
      pfd = pango_font_description_from_string("Courier,Medium 14");
   
    context = gtk_widget_get_pango_context (text);
    metrics = pango_context_get_metrics (context, pfd,
              pango_context_get_language (context));

    font_calculations_array[disp_fnt].f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
             pango_font_metrics_get_descent (metrics));	   
             
    font_calculations_array[disp_fnt].f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (metrics));
    
    
    // debug
    fprintf(stderr, " for disp_fnt  , f_height and f_width are %d and %d\n", font_calculations_array[disp_fnt].f_height,
    font_calculations_array[disp_fnt].f_width);

  }
  disp_fnt = cur_disp_fnt;  /* re-establish initial value */
  pango_font_metrics_unref (metrics); /* free these two structures */
  pango_font_description_free(pfd);
}


/* event loops */
/* configure_event currently not called, but might be included at
 * some point in the future. */
static gboolean configure_event( GtkWidget *widget,
                                 GdkEventConfigure *event )
{
/* if there was an event detected then unreference the current gr_image
   and then create a new one the size of the widget window, set fg to white
   draw white on the pixmap and then reset fg to black. */

  if (gr_image) {
   /* gdk_draw_drawable (gr_image_old,
                     widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
                     widget->window,
                     0, 0, 0, 0, -1, -1);*/
    g_object_unref(gr_image);
/* debug fprintf(stderr,"configure_event unref gr_image\n"); */
  }

  gr_image = gdk_pixmap_new(widget->window,
			    widget->allocation.width,
			    widget->allocation.height,
			    -1);
  gdk_draw_rectangle (gr_image,
		      widget->style->white_gc,
		      TRUE,
		      0, 0,widget->allocation.width,widget->allocation.height);

  /*gdk_draw_drawable (widget->window,
                     widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
                     gr_image_old,
                     0, 0, 0, 0,
		     widget->allocation.width, widget->allocation.height);
                     */
/* debug fprintf(stderr,"configure_event widget %d %d \n",widget->allocation.width,widget->allocation.height); */

  return TRUE;
}

static gboolean expose_event (GtkWidget *widget,
                              GdkEventExpose *event)
{
  gint max_w;
  gint max_h;

  max_w = widget->allocation.width;
  max_h = widget->allocation.height;

/* debug fprintf(stderr,"expose_event %d %d %d %d %d %d \n",
    max_w,max_h,event->area.x,event->area.y,event->area.width,event->area.height); */

  gdk_draw_drawable (widget->window,gc,
                     gr_image,
                     event->area.x, event->area.y,
		     event->area.x, event->area.y,
		     event->area.width, event->area.height);

  return FALSE;
}

/* Confirm quit if window destroyed by user - to be added */
void destroy( GtkWidget *widget,
              gpointer   data )
{
    /* May be add a check to see if loop running before doing this */
    g_main_loop_quit (menu_loop);
    gtk_widget_destroy (widget);	/* added so parameter used */
    exit(EXIT_SUCCESS);
}

void winfin_(void)
{
    /* May be add a check to see if loop running before doing this */
    g_main_loop_quit (menu_loop);
/*     gtk_widget_destroy (window); does the main window need to be freed? */
    exit(EXIT_SUCCESS);
}

gint delete_event( GtkWidget *widget,GdkEvent *event,gpointer data )
{
   GtkWidget *q_dialog;
   gboolean do_not_quit;
   gint result;

/* If you return FALSE in the "delete_event" signal handler,
 * GTK will emit the "destroy" signal. Returning TRUE means
 * you don't want the window to be destroyed.
 * This is useful for popping up 'are you sure you want to quit?'
 * type dialogs. */

   q_dialog = gtk_message_dialog_new (GTK_WINDOW (window),
      GTK_DIALOG_DESTROY_WITH_PARENT,GTK_MESSAGE_QUESTION,
      GTK_BUTTONS_OK_CANCEL,"Are you sure you want to quit?");
   result = gtk_dialog_run (GTK_DIALOG (q_dialog));
   switch (result)
      {
       case GTK_RESPONSE_OK:
          do_not_quit=FALSE;
          break;
       default:
          do_not_quit=TRUE;
          break;
      }
   gtk_widget_destroy (q_dialog);

    /* Change TRUE to FALSE and the main window will be destroyed with
     * a "delete_event". */

   return do_not_quit;
}


/* **** create_text() - create a scrolled text area that displays text *** */
GtkWidget *create_text( void )
{
   GtkWidget *scrolled_window;
   GtkTextBuffer *buffer;
   PangoFontDescription *pfd;	/* to hold test font */

   text = gtk_text_view_new ();

   buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
   gtk_text_view_set_editable (GTK_TEXT_VIEW (text), FALSE);
   gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (text), FALSE);

   scrolled_window = gtk_scrolled_window_new (NULL, NULL);
   gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
     GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC);

/* react to different sizes of text */
   if (disp_fnt == 0 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     g_print("create_text configure font medium 8\n");  /* debug */
   } else if (disp_fnt == 1 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     g_print("create_text configure font medium 10\n"); /* debug */
   } else if (disp_fnt == 2 ) {
     pfd = pango_font_description_from_string("Serif,Medium 12");
     g_printf("create_text configure font medium 12\n");  /*debug */
   } else if (disp_fnt == 3 ) {
     pfd = pango_font_description_from_string("Serif,Medium 14");
     g_print("create_text configure font medium 14\n"); /* debug */
   } else if (disp_fnt == 4 ) {
     pfd = pango_font_description_from_string("Courier,Medium 8");
     g_print("create_text configure courier medium 8\n"); /* debug */
   } else if (disp_fnt == 5 ) {
     pfd = pango_font_description_from_string("Courier,Medium 10");
     g_print("create_text configure courier medium 10\n"); /* debug */
   } else if (disp_fnt == 6 ) {
     pfd = pango_font_description_from_string("Courier,Medium 12");
     g_print("create_text configure courier medium 12\n"); /* debug */
   } else if (disp_fnt == 7 ) {
     pfd = pango_font_description_from_string("Courier,Medium 14");
     g_print("create_text configure courier medium 14\n"); /* debug */
   }
   gtk_widget_modify_font(text, pfd);
   pango_font_description_free(pfd);

   gtk_container_add (GTK_CONTAINER (scrolled_window), text);

   gtk_widget_show_all (scrolled_window);

   return scrolled_window;
}

/* egphelp_disp() - display help text via egphelpscroll_ */
void egphelp_disp ( void)
{
 long int IBX, IBY,IPFLG,more,iuresp;	/* dummy parameters */

 IBX=0; IBY=0; IPFLG=0; more=0; iuresp=0;
 egphelpscroll_(&IBX,&IBY,&IPFLG,&more,&iuresp);

}

/* copyright_disp() - display help text in a new window via call to cpwpk_
 * this is a call-back function from the static menu structure. */
void copyright_disp ( void)
{
   long int avail_cpw;
   avail_cpw = 1;
   cpwpk_(&avail_cpw);
}

/* esru_ask_disp() - display help text in a new window via call to ask_about_
 * this is a call-back function from the static menu structure. This
 * wrapper is needed because askabout takes parameters. */
void esru_ask_disp ( void)
{
   char buf[5];
   long int initialit;
   initialit = 0;
   strcpy(buf,cappl);	/* copy global text variable for current application to buf */
   askabout_(buf,&initialit);
}

/* esru_ask_wire() - display wireframe help text in a new window via call to ask_about_
 * this is a call-back function. This wrapper is needed because askabout takes parameters. */
void esru_ask_wire ( void)
{
   char buf[5];
   long int initialit;
   initialit = 1;
   strcpy(buf,"wire");	/* copy global text variable for current application to buf */
   askabout_(buf,&initialit);
}

/* text_feedback_reset() - called from the text feedback display font reset
 * callbacks to pass information back to fortran via updview_. */
void text_feedback_reset ( void)
{

  PangoFontDescription *pfd;	/* to hold test font */
   
   gint g_width,g_height;	/* size of the graphics widget */
   long int b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
   gint textf_pix_ht;	/* pixel height of text feedback (based on number of lines requested) */
   long int ifsc,itfsc,imfsc,lttyc; /* parameters must be long int */
   long int gw,gh,gdw,gdh,g3w,g3h;             /* to match fortran conventions */
   long int cl,cr,ct,cb,vl,vr,vt,vb;

/* First find the size of the graphic window. Note: do this prior to changing
 * the text feedback display font by using Pango context previously setup in esp-r.c */
    
   gdk_drawable_get_size(graphic->window,&g_width,&g_height);
   fprintf(stderr,"std font height width is %d %d gr_w %d gr_h %d\n", f_height,f_width,g_width,g_height); /* debug */
   b_top = (long int) (0 + (f_height * c3dct));	/* pixel @ top    */
   b_bottom = (long int) (g_height - 9 - (f_height * c3dcb));	/* pixel @ bottom */
   b_left = (long int) (0 + 9 + (f_width * c3dcl));	/* pixel @ left   */
   b_right = (long int) (g_width - (f_width * c3dcr));	/* pixel @ right  */
   gh = b_bottom - b_top;  /* box within axis pixel height  */
   gw = b_right - b_left;  /* box within axis pixel width */
   fprintf(stderr,"other box info b_top %ld b_bottom %ld b_left %ld b_right %ld\n",b_top,b_bottom,b_left,b_right); /* debug */
   fprintf(stderr,"box within axis gh %ld gw %ld\n",gh,gw); /* debug */

/* Originally there were three options for fonts and disp_fnt was either 0, 1, or 2.
 * With the addition of Serif Largest, Courier Small, Courier Medium, Courier Large and Courier Largest,
 * the disp_fnt also has the values 3, 4, 5, 6, 7 
 * << NOTE: the fortran side does not know about these new values >>
 */

/* Read the calculated values from the array depending on value of disp_fnt */
   if (disp_fnt == 0 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
     f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
 
     fprintf(stderr,"text_feedback_reset at serif medium 8 change font height and width is %d %d", f_height, f_width);	//debug
    
     
   } else if (disp_fnt == 1 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     f_height = font_calculations_array[serif_medium].f_height;
     f_width  = font_calculations_array[serif_medium].f_width;
 
     fprintf(stderr,"text_feedback_reset at serif medium 10 change font height and width is %d %d", f_height, f_width);	//debug
      
   } else if (disp_fnt == 2 ) {
     pfd = pango_font_description_from_string("Serif,Medium 12");
     f_height = font_calculations_array[serif_large].f_height;
     f_width  = font_calculations_array[serif_large].f_width;
 
     fprintf(stderr,"text_feedback_reset at serif medium 12 change font height and width is %d %d", f_height, f_width);	//debug	

   } else if (disp_fnt == 3 ) {
     pfd = pango_font_description_from_string("Serif,Medium 14");
     f_height = font_calculations_array[serif_largest].f_height;
     f_width  = font_calculations_array[serif_largest].f_width;
 
     fprintf(stderr,"text_feedback_reset at serif medium 14 change font height and width is %d %d", f_height, f_width);	//debug	
   }
   
   else if (disp_fnt == 4 ) {
     pfd = pango_font_description_from_string("Courier,Medium 8");
     f_height = font_calculations_array[courier_small].f_height;
     f_width  = font_calculations_array[courier_small].f_width;
 
     fprintf(stderr,"text_feedback_reset at courier medium 8 change font height and width is %d %d", f_height, f_width);	//debug	 
      
   } 
   else if (disp_fnt == 5 ) {
     pfd = pango_font_description_from_string("Courier,Medium 10");
     f_height = font_calculations_array[courier_medium].f_height;
     f_width  = font_calculations_array[courier_medium].f_width;
 
     fprintf(stderr,"text_feedback_reset at courier medium 10 change font height and width is %d %d", f_height, f_width);	//debug	 
   }
    else if (disp_fnt == 6 ) {
     pfd = pango_font_description_from_string("Courier,Medium 12");
     f_height = font_calculations_array[courier_large].f_height;
     f_width  = font_calculations_array[courier_large].f_width;
 
     fprintf(stderr,"text_feedback_reset at courier medium 12 change font height and width is %d %d", f_height, f_width);	//debug	  
   }
   else if (disp_fnt == 7 ) {
     pfd = pango_font_description_from_string("Courier,Medium 14");
     f_height = font_calculations_array[courier_largest].f_height;
     f_width  = font_calculations_array[courier_largest].f_width;
 
     fprintf(stderr,"text_feedback_reset at courier medium 14 change font height and width is %d %d", f_height, f_width);	//debug	 
   }
   
   gtk_widget_modify_font(text, pfd);
   
   fprintf(stderr,"text_feedback_reset text feedback pixels %d %d \n",text->allocation.width,text->allocation.height); /* debug */
 
   pango_font_description_free(pfd);

   ifsc=butn_fnt; itfsc=disp_fnt; imfsc=menu_fnt;
   lttyc=10;	/* << this is a place holder >> */

/* pass revised information back to the fortran side */
   updview_(&ifsc,&itfsc,&imfsc,&b_left,&b_right,&b_top,&b_bottom,&gw,&gh,&lttyc);

}

/* emenu_feedback_reset() - called from the menu display font reset
 * callbacks to pass information back to fortran via updview_. */
void emenu_feedback_reset ( void)
{
   PangoFontDescription *pfd;	/* to hold test font */
   PangoContext *context;	/* for use in finding font properties */
   PangoFontMetrics *metrics;
   gint f_height;	/* pixel height of default font */
   gint f_width;	/* pixel width of default font */
   gint g_width,g_height;	/* size of the graphics widget */
   long int b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
   gint textf_pix_ht;	/* pixel height of text feedback (based on number of lines requested) */
   long int ifsc,itfsc,imfsc,lttyc; /* parameters must be long int */
   long int gw,gh,gdw,gdh,g3w,g3h;             /* to match fortran conventions */
   long int cl,cr,ct,cb,vl,vr,vt,vb;

/* the following would not be necessary if graphicf extents were remembered globally */
/* << fix this >> */
/* First find the size of the graphic window. Note: do this prior to changing
 * the text feedback display font by using Pango context previously setup in esp-r.c */
   f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics) +
            pango_font_metrics_get_descent (pango_metrics));
   f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (pango_metrics));
   gdk_drawable_get_size(graphic->window,&g_width,&g_height);
   fprintf(stderr,"std font height width is %d %d gr_w %d gr_h %d\n", f_height,f_width,g_width,g_height); /* debug */
   b_top = (long int) (0 + (f_height * c3dct));	/* pixel @ top    */
   b_bottom = (long int) (g_height - 9 - (f_height * c3dcb));	/* pixel @ bottom */
   b_left = (long int) (0 + 9 + (f_width * c3dcl));	/* pixel @ left   */
   b_right = (long int) (g_width - (f_width * c3dcr));	/* pixel @ right  */
   gh = b_bottom - b_top;  /* box within axis pixel height  */
   gw = b_right - b_left;  /* box within axis pixel width */
   fprintf(stderr,"other box info b_top %ld b_bottom %ld b_left %ld b_right %ld\n",b_top,b_bottom,b_left,b_right); /* debug */
   fprintf(stderr,"box within axis gh %ld gw %ld\n",gh,gw); /* debug */
   fprintf(stderr,"emenu_feedback_reset text feedback pixels %d %d \n",text->allocation.width,text->allocation.height); /* debug */

/* create font description to use for resetting the menu font << dosn't quite work >>.
 * Use pango_font_get_metrics to get font width and height.
 *  if (menu_fnt == 0 ) {
 *    pfd = pango_font_description_from_string("Serif,Medium 8");
 *    fprintf(stderr,"re-configure font medium 8\n");
 *  } else if (menu_fnt == 1 ) {
 *    pfd = pango_font_description_from_string("Serif,Medium 10");
 *    fprintf(stderr,"re-configure font medium 10\n");
 *  } else if (menu_fnt == 2 ) {
 *    pfd = pango_font_description_from_string("Serif,Medium 12");
 *    fprintf(stderr,"re-configure font medium 12\n");
 *  }
 *  gtk_widget_modify_font(emenu, pfd);
 *  pango_font_description_free(pfd);
 */
   ifsc=butn_fnt; itfsc=disp_fnt; imfsc=menu_fnt;
   lttyc=10;	/* << this is a place holder >> */

/* pass revised information back to the fortran side */
   updview_(&ifsc,&itfsc,&imfsc,&b_left,&b_right,&b_top,&b_bottom,&gw,&gh,&lttyc);

}

/* graphic_feedback_reset() - called from the graphic font reset
 * callbacks to pass information back to fortran via updview_. */
void graphic_feedback_reset ( void)
{
   PangoFontDescription *pfd;	/* to hold test font */
   PangoContext *context;	/* for use in finding font properties */
   PangoFontMetrics *metrics;
   gint f_height;	/* pixel height of default font */
   gint f_width;	/* pixel width of default font */
   gint g_width,g_height;	/* size of the graphics widget */
   long int b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
   gint textf_pix_ht;	/* pixel height of text feedback (based on number of lines requested) */
   long int ifsc,itfsc,imfsc,lttyc; /* parameters must be long int */
   long int gw,gh,gdw,gdh,g3w,g3h;             /* to match fortran conventions */
   long int cl,cr,ct,cb,vl,vr,vt,vb;

/* Create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height. */
   if (butn_fnt == 0 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     /* fprintf(stderr,"re-configure graphic font medium 8\n");	debug */
   } else if (butn_fnt == 1 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     /* fprintf(stderr,"re-configure graphic font medium 10\n"); debug */
   } else if (butn_fnt == 2 ) {
     pfd = pango_font_description_from_string("Serif,Medium 12");
     /* fprintf(stderr,"re-configure graphic font medium 12\n"); debug */
   }
   gtk_widget_modify_font(graphic, pfd);	/* << ?? >> */
   context = gtk_widget_get_pango_context (graphic);
   metrics = pango_context_get_metrics (context, pfd,
             pango_context_get_language (context));
   f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
           pango_font_metrics_get_descent (metrics));
   f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (metrics));
   pango_font_metrics_unref (metrics);
   pango_font_description_free(pfd);

   gdk_drawable_get_size(graphic->window,&g_width,&g_height);
   fprintf(stderr,"graphic font height width is %d %d gr_w %d gr_h %d\n", f_height,f_width,g_width,g_height); /* debug */
   b_top = (long int) (0 + (f_height * c3dct));	/* pixel @ top    */
   b_bottom = (long int) (g_height - 9 - (f_height * c3dcb));	/* pixel @ bottom */
   b_left = (long int) (0 + 9 + (f_width * c3dcl));	/* pixel @ left   */
   b_right = (long int) (g_width - (f_width * c3dcr));	/* pixel @ right  */
   gh = b_bottom - b_top;  /* box within axis pixel height  */
   gw = b_right - b_left;  /* box within axis pixel width */
   fprintf(stderr,"graphic box info b_top %ld b_bottom %ld b_left %ld b_right %ld\n",b_top,b_bottom,b_left,b_right); /* debug */
   fprintf(stderr,"box within axis gh %ld gw %ld\n",gh,gw); /* debug */
   fprintf(stderr,"graphic_feedback_reset text feedback pixels %d %d \n",text->allocation.width,text->allocation.height); /* debug */

   ifsc=butn_fnt; itfsc=disp_fnt; imfsc=menu_fnt;
   lttyc=10;	/* << this is a place holder >> */

/* pass revised information back to the fortran side */
   updview_(&ifsc,&itfsc,&imfsc,&b_left,&b_right,&b_top,&b_bottom,&gw,&gh,&lttyc);

}

/* **** sml_menu_fbk_cb - callback function from small/medium/large text feedback font */
void sml_menu_fbk_cb (GtkWidget *widget, gpointer resize)
{
   /* debug fprintf(stderr,"** sml_menu_fbk_cb %d\n", GPOINTER_TO_INT (resize)); */
   if (disp_fnt != GPOINTER_TO_INT (resize)) {
     disp_fnt = GPOINTER_TO_INT (resize);
     text_feedback_reset();
   }
}

/* **** Define small/medium/large option menu for use in feedback font resizing */
static GtkWidget *sml_menu_fbk ( void)
{
   GtkWidget *sml_menu;
   GtkWidget *menuitem;
   GSList *group;

   sml_menu = gtk_menu_new ();
   group = NULL;
   
   /* There is a Serif submenu and a Courier submenu. First create the Serif label: */
   menuitem= gtk_menu_item_new_with_label ("Serif");
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   gtk_widget_show (menuitem);
   
   menuitem = gtk_radio_menu_item_new_with_label (group,"small");              // creation of radio button
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));     // assigning radio button to its group
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);		       // adds the menuitem to the end of the menu shell's item list
   g_signal_connect (G_OBJECT (menuitem), "activate",			       // when radio button is activated, set GINT_TO_POINTER (0)
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (0));       // therefore disp_font == 0
   gtk_widget_show (menuitem);						      // display the radio button

   menuitem = gtk_radio_menu_item_new_with_label (group,"medium");		
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));	
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (1));
   gtk_widget_show (menuitem);

   menuitem = gtk_radio_menu_item_new_with_label (group,"large");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (2));
   gtk_widget_show (menuitem);
   
   menuitem = gtk_radio_menu_item_new_with_label (group,"largest");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (3));
   gtk_widget_show (menuitem);
   
   /* Separator between Serif and Courier sections */
   menuitem= gtk_menu_item_new_with_label ("-----------");
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   gtk_widget_show (menuitem);
   
   
   /* "Courier" label in the submenu */
   menuitem= gtk_menu_item_new_with_label ("Courier");
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   gtk_widget_show (menuitem);
  
   menuitem = gtk_radio_menu_item_new_with_label (group,"small ");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (4));
   gtk_widget_show (menuitem);

   menuitem = gtk_radio_menu_item_new_with_label (group,"medium");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (5));
   gtk_widget_show (menuitem);

   menuitem = gtk_radio_menu_item_new_with_label (group,"large ");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (6));
   gtk_widget_show (menuitem);
   
    menuitem = gtk_radio_menu_item_new_with_label (group,"largest");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_fbk_cb), GINT_TO_POINTER (7));
   gtk_widget_show (menuitem);

   return sml_menu;
}

/* **** sml_menu_gph_cb - callback function from small/medium/large text graphics font */
void sml_menu_gph_cb (GtkWidget *widget, gpointer resize)
{
   /* debug fprintf(stderr,"** sml_menu_gph_cb %d\n", GPOINTER_TO_INT (resize)); */
   if (butn_fnt != GPOINTER_TO_INT (resize)) {
     butn_fnt = GPOINTER_TO_INT (resize);
     graphic_feedback_reset();
   }
}

/* **** Define small/medium/large option menu for use in graphics font resizing */
static GtkWidget *sml_menu_gph ( void)
{
   GtkWidget *sml_menu;
   GtkWidget *menuitem;
   GSList *group;

   sml_menu = gtk_menu_new ();
   group = NULL;

   menuitem = gtk_radio_menu_item_new_with_label (group,"small");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_gph_cb), GINT_TO_POINTER (0));
   gtk_widget_show (menuitem);

   menuitem = gtk_radio_menu_item_new_with_label (group,"medium");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_gph_cb), GINT_TO_POINTER (1));
   gtk_widget_show (menuitem);

   menuitem = gtk_radio_menu_item_new_with_label (group,"large");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_gph_cb), GINT_TO_POINTER (2));
   gtk_widget_show (menuitem);

   return sml_menu;
}

/* **** sml_menu_mnu_cb - callback function from small/medium/large text menu font */
void sml_menu_mnu_cb (GtkWidget *widget, gpointer resize)
{
   /* debug fprintf(stderr,"** sml_menu_mnu_cb %d\n", GPOINTER_TO_INT (resize)); */
   if (menu_fnt != GPOINTER_TO_INT (resize)) {
     menu_fnt = GPOINTER_TO_INT (resize);
     emenu_feedback_reset();
   }
}

/* **** Define small/medium/large option menu for use in menu font resizing */
static GtkWidget *sml_menu_mnu ( void)
{
   GtkWidget *sml_menu;
   GtkWidget *menuitem;
   GSList *group;

   sml_menu = gtk_menu_new ();
   group = NULL;

   menuitem = gtk_radio_menu_item_new_with_label (group,"small");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_mnu_cb), GINT_TO_POINTER (0));
   gtk_widget_show (menuitem);

   menuitem = gtk_radio_menu_item_new_with_label (group,"medium");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_mnu_cb), GINT_TO_POINTER (1));
   gtk_widget_show (menuitem);

   menuitem = gtk_radio_menu_item_new_with_label (group,"large");
   group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
   gtk_menu_shell_append (GTK_MENU_SHELL (sml_menu), menuitem);
   g_signal_connect (G_OBJECT (menuitem), "activate",
                     G_CALLBACK (sml_menu_mnu_cb), GINT_TO_POINTER (2));
   gtk_widget_show (menuitem);

   return sml_menu;
}

/* **** menuitem_response() - print a string when a menu item is selected
 * it is a placeholder until such as as we think of something else. */
static void menuitem_response( gchar *string )
{
  printf ("%s\n", string);
}

/* esru_rotate_left() - send message to fortran to rotate wireframe left. */
void esru_rotate_left ( void)
{
  long int azichange;	/* current value to pass to fortran. */
  azichange = 10;	/* visual clockwise */
  chgazi_(&azichange);  /* Deal with user selection of azimuth decrement  */
}

/* esru_rotate_right() - send message to fortran to rotate wireframe left. */
void esru_rotate_right ( void)
{
  long int azichange;	/* current value to pass to fortran. */
  azichange = -10;	/* visual anticlockwise */
  chgazi_(&azichange);  /* Deal with user selection of azimuth decrement  */
}

/* esru_elev_up() - send message to fortran to rotate wireframe up. */
void esru_elev_up ( void)
{
  long int elevchange;	/* current value to pass to fortran. */
  elevchange = 10;	/* visual up */
  chgelev_(&elevchange);  /* Deal with user selection of azimuth decrement  */
}

/* esru_elev_down() - send message to fortran to rotate wireframe down. */
void esru_elev_down ( void)
{
  long int elevchange;	/* current value to pass to fortran. */
  elevchange = -10;	/* visual up */
  chgelev_(&elevchange);  /* Deal with user selection of azimuth decrement  */
}

/* esru_wire_pick() - send message to fortran to rotate wireframe down. */
void esru_wire_pick ( void)
{
  long int avail_wire;	/* current value of wire_avail to pass to fortran. */
  avail_wire = wire_avail;
  wirepk_(&avail_wire);  /* Deal with user selection of wireframe control  */
}

/* esru_wire_tog() - send message to fortran to rotate wireframe down. */
void esru_wire_tog ( void)
{
  long int avail_wire;	/* current value of wire_avail to pass to fortran. */
  avail_wire = wire_avail;
  wiretog_(&avail_wire);  /* Deal with user selection of wireframe control  */
}

/* putzonename(name) - add name to zonenames char array */
void putzonename_ (char* name, long int* id,  int length)
{
   gchar *name_local;
   int zoneid = (int) *id;

   name_local = g_strndup(name, (gsize) length);
   //fprintf(stderr,"number of zone %d\n", zoneid);
   //fprintf(stderr,"name of zone %s\n",name_local);
   /* copy local string to char array*/
   strcpy(zonenames[zoneid-1],name_local);
   //fprintf(stderr,"zone name%s\n",zonenames[zoneid-1]);
}

/* esru_wire_ctl() - wireframe control.
    This routine alters the contents of the following common block/struct directly:
      ray2
      image
    On pressing 'apply' the fortran routine GDUPDATE is called.
    On pressing 'OK' GDUPDATE is called and the dialog closed.*/
void esru_wire_ctl ( void)
{
   /* Main popup widget */
   GtkWidget *control;

   /* Generic display wigets - reused several times */
   GtkWidget *hbox, *vbox, *label, *spinner, *frame, *table;

   /* Specific widgets - each will have a value passed to and from fortran */
   GtkObject *EyePointValueX, *EyePointValueY, *EyePointValueZ;
   GtkObject *ViewPointValueX, *ViewPointValueY, *ViewPointValueZ;
   GtkWidget *ViewBoundOptimum;
   GtkObject *ViewAngleValue;
   GtkWidget *DisplayZoneName, *DisplaySurfaceName, *DisplayVertexNumbers;
   GtkWidget *DisplaySurfaceNormals, *DisplaySiteGrid, *DisplaySiteOrigin;
   GtkWidget *GridSpacingOtimum;
   GtkObject *GridSpacingDistance;
   GtkWidget *ViewPerspective, *ViewPlan, *ViewEastElev, *ViewNorthElev, *ViewFromSun;
   GtkWidget *zone_button[MCOM];
   GtkWidget *IncludeAll, *IncludeSurfaces, *IncludeExternal, *IncludePartition;
   GtkWidget *IncludeSimilar, *IncludeSurObsGrnd, *IncludeGrnd;
   GtkWidget *HighDefault, *HighConstr, *HighOpaque, *HighTrans, *HighPartAtt;

   gint nrows, irow, icol, izone, itchar, ichar, result;
   int no_valid_event;
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */

   esru_ask_wire();	/* instanciate the help associated with this interface */

   control = gtk_dialog_new_with_buttons("Wireframe control",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,
     GTK_STOCK_APPLY, GTK_RESPONSE_APPLY,
     GTK_STOCK_OK, GTK_RESPONSE_OK,NULL);

   /* create eye point dialog (label and three spinner boxes) */
   frame = gtk_frame_new ("View orientation");
   gtk_container_set_border_width (GTK_CONTAINER (frame), 2);
   vbox = gtk_vbox_new (FALSE, 1);
   hbox = gtk_hbox_new (FALSE, 1);
   label = gtk_label_new ("Eye point (x,y,z):");
   gtk_container_add (GTK_CONTAINER (hbox),label);
   EyePointValueX = gtk_adjustment_new ((gdouble) image_.EYEM[0], -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (EyePointValueX), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   EyePointValueY = gtk_adjustment_new ((gdouble) image_.EYEM[1], -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (EyePointValueY), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   EyePointValueZ = gtk_adjustment_new ((gdouble) image_.EYEM[2], -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (EyePointValueZ), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   gtk_container_add (GTK_CONTAINER (vbox),hbox);

   /* create view point dialog (label and three spinner boxes) */
   hbox = gtk_hbox_new (FALSE, 1);
   label = gtk_label_new ("View point (x,y,z):");
   gtk_container_add (GTK_CONTAINER (hbox),label);
   ViewPointValueX = gtk_adjustment_new ((gdouble) image_.VIEWM[0], -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (ViewPointValueX), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   ViewPointValueY = gtk_adjustment_new ((gdouble) image_.VIEWM[1], -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (ViewPointValueY), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   ViewPointValueZ = gtk_adjustment_new ((gdouble) image_.VIEWM[2], -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (ViewPointValueZ), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   gtk_container_add (GTK_CONTAINER (vbox),hbox);

   /* create view bound dialog (radio buttons with spinner box) */
   hbox = gtk_hbox_new (FALSE, 1);
   label = gtk_label_new ("View bounds:");
   gtk_container_add (GTK_CONTAINER (hbox),label);
   ViewBoundOptimum = gtk_check_button_new_with_label ("optimum");
   gtk_container_add (GTK_CONTAINER (hbox),ViewBoundOptimum);
   if (ray2_.ITBND == 1 ) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ViewBoundOptimum), TRUE);
   }
   label = gtk_label_new ("angle (deg):");
   gtk_container_add (GTK_CONTAINER (hbox),label);
   ViewAngleValue = gtk_adjustment_new ((gdouble) image_.ANG, 0.001, 90., 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (ViewAngleValue), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), FALSE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   gtk_container_add (GTK_CONTAINER (vbox),hbox);
   gtk_container_add (GTK_CONTAINER (frame),vbox);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(control)->vbox),frame);

   /* create a table of display toggles */
   frame = gtk_frame_new ("Display information toggles");
   gtk_container_set_border_width (GTK_CONTAINER (frame), 4);
   /* set number of rows and columns */
   table = gtk_table_new (2, 3, TRUE);
   gtk_container_add (GTK_CONTAINER (frame), table);
   DisplayZoneName = gtk_check_button_new_with_label ("Zone names");
   /* attach to column and row (note different order to above!) */
   gtk_table_attach_defaults (GTK_TABLE (table), DisplayZoneName, 0, 1, 0, 1);
   if (ray2_.ITZNM == 0 ) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (DisplayZoneName), TRUE);
   }
   DisplaySurfaceName = gtk_check_button_new_with_label ("Surface names");
   gtk_table_attach_defaults (GTK_TABLE (table), DisplaySurfaceName, 0, 1, 1, 2);
   if (ray2_.ITSNM == 0 ) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (DisplaySurfaceName), TRUE);
   }
   DisplayVertexNumbers = gtk_check_button_new_with_label ("Vertex numbers");
   gtk_table_attach_defaults (GTK_TABLE (table), DisplayVertexNumbers, 1, 2, 0, 1);
   if (ray2_.ITVNO == 0 ) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (DisplayVertexNumbers), TRUE);
   }
   DisplaySurfaceNormals = gtk_check_button_new_with_label ("Surface normals");
   gtk_table_attach_defaults (GTK_TABLE (table), DisplaySurfaceNormals, 1, 2, 1, 2);
   if (ray2_.ITSNR == 0 ) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (DisplaySurfaceNormals), TRUE);
   }
   DisplaySiteGrid = gtk_check_button_new_with_label ("Site grid");
   gtk_table_attach_defaults (GTK_TABLE (table), DisplaySiteGrid, 2, 3, 0, 1);
   if (ray2_.ITGRD == 0 ) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (DisplaySiteGrid), TRUE);
   }
   DisplaySiteOrigin = gtk_check_button_new_with_label ("Site origin");
   gtk_table_attach_defaults (GTK_TABLE (table), DisplaySiteOrigin, 2, 3, 1, 2);
   if (ray2_.ITORG == 0 ) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (DisplaySiteOrigin), TRUE);
   }
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(control)->vbox),frame);

  /* create display grid dialog (radio buttons with spinner box) */
   frame = gtk_frame_new ("Display grid");
   gtk_container_set_border_width (GTK_CONTAINER (frame), 4);
   hbox = gtk_hbox_new (FALSE, 1);
   gtk_container_add (GTK_CONTAINER (frame), hbox);
   label = gtk_label_new ("Spacing:");
   gtk_container_add (GTK_CONTAINER (hbox),label);
   GridSpacingOtimum = gtk_check_button_new_with_label ("optimum");
   gtk_container_add (GTK_CONTAINER (hbox),GridSpacingOtimum);
   label = gtk_label_new (" distance (m):");
   gtk_container_add (GTK_CONTAINER (hbox),label);
   GridSpacingDistance = gtk_adjustment_new ((gdouble) ray2_.GRDIS, -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
   spinner = gtk_spin_button_new (GTK_ADJUSTMENT (GridSpacingDistance), 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (hbox),spinner);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(control)->vbox),frame);

   /* create zones to display list  <<pick up zone names here>>*/
   nrows = 1 + c1_.NCOMP/4;
   irow = 0;
   //fprintf(stderr,"number of zones %d\n",c1_.NCOMP);
   //fprintf(stderr,"number of rows %d\n",nrows);
   //fprintf(stderr,"number of rows %f\n",nrows);
   frame = gtk_frame_new ("Zones to display");
   gtk_container_set_border_width (GTK_CONTAINER (frame), 4);
   table = gtk_table_new (nrows, 4, TRUE);
   gtk_container_add (GTK_CONTAINER (frame), table);
   getzonenames_();
   izone = 0;
   while (izone < c1_.NCOMP) {
     for(icol = 0; icol < 4; icol++) {	/* fill columns in order  <<why does the while loop not work?>>*/
       fprintf(stderr,"Zone %d, col %d, row %d\n",izone+1, icol, irow);
       zone_button[izone] = gtk_check_button_new_with_label (zonenames[izone]);
       gtk_table_attach_defaults (GTK_TABLE (table), zone_button[izone], icol, icol+1, irow, irow+1);
       izone++;
     }
     irow++;
   }
   /* Set active zones.  Remember that C starts at 0, therefore zone number 2 is number 1 here!
      This means that the zone referenced in the fortran
      code is one more than in the c code (hence the -1 below).
   */
   for (izone = 0; izone < gzonpik_.nzg; izone++) {
     gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (zone_button[gzonpik_.nznog[izone]-1]), TRUE);
   }
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(control)->vbox),frame);


   /* create a table of display toggles */
   frame = gtk_frame_new ("Display options");
   gtk_container_set_border_width (GTK_CONTAINER (frame), 4);
   /* set number of rows and columns */
   table = gtk_table_new (3, 2, FALSE);
   gtk_container_add (GTK_CONTAINER (frame), table);
   label = gtk_label_new ("View type:");
   /* attach to column and row (note different order to above!) */
   gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 0, 1);
   hbox = gtk_hbox_new (FALSE, 1);
/*    gtk_container_add (GTK_CONTAINER (hbox),label); this causes a GTK warning about a label widget inside a table */

/* Use variable ray2_.ITPPSW = 0 for perspective, ray2_.ITPPSW = 1 for plan,
 * ray2_.ITPPSW = 2 for south elevation, ray2_.ITPPSW = 2 for east elevation
 * to drive gtk_toggle_button_set_active call
 */
   ViewPerspective = gtk_radio_button_new_with_label (NULL, "Perspective");
   gtk_container_add (GTK_CONTAINER (hbox),ViewPerspective);
   if (ray2_.ITPPSW == 0) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ViewPerspective), TRUE);
   }
   ViewPlan = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (ViewPerspective), "Plan");
   gtk_container_add (GTK_CONTAINER (hbox),ViewPlan);
   if (ray2_.ITPPSW == 1) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ViewPlan), TRUE);
   }
   ViewEastElev = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (ViewPerspective), "East elevation");
   gtk_container_add (GTK_CONTAINER (hbox),ViewEastElev);
   if (ray2_.ITPPSW == 3) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ViewEastElev), TRUE);
   }
   ViewNorthElev = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (ViewPerspective), "South elevation");
   gtk_container_add (GTK_CONTAINER (hbox),ViewNorthElev);
   if (ray2_.ITPPSW == 2) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ViewNorthElev), TRUE);
   }
   ViewFromSun = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (ViewPerspective), "View from sun");
   gtk_container_add (GTK_CONTAINER (hbox),ViewFromSun);
   if (ray2_.ITPPSW == 4) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ViewFromSun), TRUE);
   }
   gtk_table_attach_defaults (GTK_TABLE (table), hbox, 1, 2, 0, 1);
   label = gtk_label_new ("Include:");
   gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 1, 2);
   hbox = gtk_hbox_new (FALSE, 1);
/*    gtk_container_add (GTK_CONTAINER (hbox),label); this causes a GTK warning about a label widget inside a table */

   IncludeAll = gtk_radio_button_new_with_label (NULL,"Surf+Obs");
   gtk_container_add (GTK_CONTAINER (hbox),IncludeAll);
   if (ray2_.ITDSP == 0) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(IncludeAll), TRUE);
   }
   IncludeSurfaces = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (IncludeAll), "Surf");
   gtk_container_add (GTK_CONTAINER (hbox),IncludeSurfaces);
   if (ray2_.ITDSP == 1) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(IncludeSurfaces), TRUE);
   }
   IncludeExternal = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (IncludeAll), "Extrn");
   gtk_container_add (GTK_CONTAINER (hbox),IncludeExternal);
   if (ray2_.ITDSP == 2) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(IncludeExternal), TRUE);
   }
   IncludePartition = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (IncludeAll), "Partn");
   gtk_container_add (GTK_CONTAINER (hbox),IncludePartition);
   if (ray2_.ITDSP == 3) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(IncludePartition), TRUE);
   }
   IncludeSimilar = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (IncludeAll), "Similar");
   gtk_container_add (GTK_CONTAINER (hbox),IncludeSimilar);
   if (ray2_.ITDSP == 4) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(IncludeSimilar), TRUE);
   }
   IncludeSurObsGrnd = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (IncludeAll), "Surf+Obs+Grnd");
   gtk_container_add (GTK_CONTAINER (hbox),IncludeSurObsGrnd);
   if (ray2_.ITDSP == 5) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(IncludeSurObsGrnd), TRUE);
   }
   IncludeGrnd = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (IncludeAll), "Ground");
   gtk_container_add (GTK_CONTAINER (hbox),IncludeGrnd);
   if (ray2_.ITDSP == 6) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(IncludeGrnd), TRUE);
   }

   gtk_table_attach_defaults (GTK_TABLE (table), hbox, 1, 2, 1, 2);
   label = gtk_label_new ("Highlight:");
   gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 2, 3);
   hbox = gtk_hbox_new (FALSE, 1);
/*    gtk_container_add (GTK_CONTAINER (hbox),label); this causes a GTK warning about a label widget inside a table */
   HighDefault = gtk_radio_button_new_with_label (NULL,"Default");
   gtk_container_add (GTK_CONTAINER (hbox),HighDefault);
   if (ray2_.ITHLS == 0) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(HighDefault), TRUE);
   }
   HighConstr = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (HighDefault), "By construction");
   gtk_container_add (GTK_CONTAINER (hbox),HighConstr);
   if (ray2_.ITHLS == 1) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(HighConstr), TRUE);
   }
   HighOpaque = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (HighDefault), "Opaque");
   gtk_container_add (GTK_CONTAINER (hbox),HighOpaque);
   if ((ray2_.ITHLS == 2) && (ray2_.ITHLZ == 1)) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(HighOpaque), TRUE);
   }
   HighTrans = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (HighDefault), "Transparent");
   gtk_container_add (GTK_CONTAINER (hbox),HighTrans);
   if ((ray2_.ITHLS == 2) && (ray2_.ITHLZ == 2)) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(HighTrans), TRUE);
   }
   HighPartAtt = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (HighDefault), "Partially attributed");
   gtk_container_add (GTK_CONTAINER (hbox),HighPartAtt);
   if (ray2_.ITHLS == 3) {
     gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(HighPartAtt), TRUE);
   }
   gtk_table_attach_defaults (GTK_TABLE (table), hbox, 1, 2, 2, 3);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(control)->vbox),frame);
   gtk_widget_show_all (control);


   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (control), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (control), GTK_WINDOW (window));

   /* Run this as a while loop 'no_valid-event' so we can call other widgets, for
      example, a help dialog directly from here */
   no_valid_event = TRUE;
   while ( no_valid_event ) {
     result = gtk_dialog_run (GTK_DIALOG (control));
     switch (result) {
       case GTK_RESPONSE_OK:
         image_.EYEM[0] = gtk_adjustment_get_value(GTK_ADJUSTMENT (EyePointValueX));
         image_.EYEM[1] = gtk_adjustment_get_value(GTK_ADJUSTMENT (EyePointValueY));
         image_.EYEM[2] = gtk_adjustment_get_value(GTK_ADJUSTMENT (EyePointValueZ));
         image_.VIEWM[0] = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewPointValueX));
         image_.VIEWM[1] = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewPointValueY));
         image_.VIEWM[2] = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewPointValueZ));
         image_.ANG = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewAngleValue));
         image_.HANG = image_.ANG/2.;
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplayZoneName)) == TRUE) {
           ray2_.ITZNM = 0;} else {ray2_.ITZNM = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySurfaceName)) == TRUE) {
           ray2_.ITSNM = 0;} else {ray2_.ITSNM = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplayVertexNumbers)) == TRUE) {
           ray2_.ITVNO = 0;} else {ray2_.ITVNO = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySurfaceNormals)) == TRUE) {
           ray2_.ITSNR = 0;} else {ray2_.ITSNR = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySiteGrid)) == TRUE) {
           ray2_.ITGRD = 0;} else {ray2_.ITGRD = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySiteOrigin)) == TRUE) {
           ray2_.ITORG = 0;} else {ray2_.ITORG = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (GridSpacingOtimum)) == TRUE) {
           ray2_.GRDIS = 0.;} else {ray2_.GRDIS = gtk_adjustment_get_value(GTK_ADJUSTMENT (GridSpacingDistance));}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (ViewBoundOptimum)) == TRUE) {
           ray2_.ITBND = 1;} else {ray2_.ITBND = 0;}
         /* Remember that C starts at zero, so add one to the zoneid to get the correct value
            for the fortran code.
         */
         gzonpik_.nzg = 0;
         for (izone = 0; izone < c1_.NCOMP; izone++) {
           if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (zone_button[izone])) == TRUE) {
             gzonpik_.nznog[gzonpik_.nzg++] = izone+1;}
         }
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeAll)) == TRUE) {
           ray2_.ITDSP = 0;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeSurfaces)) == TRUE) {
           ray2_.ITDSP = 1;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeExternal)) == TRUE) {
           ray2_.ITDSP = 2;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludePartition)) == TRUE) {
           ray2_.ITDSP = 3;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeSimilar)) == TRUE) {
           ray2_.ITDSP = 4;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeSurObsGrnd)) == TRUE) {
           ray2_.ITDSP = 5;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeGrnd)) == TRUE) {
           ray2_.ITDSP = 6;}

         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighDefault)) == TRUE) {
           ray2_.ITHLS = 0;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighConstr)) == TRUE) {
           ray2_.ITHLS = 1;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighOpaque)) == TRUE) {
           ray2_.ITHLS = 2;	/* Signal opaque by setting both ITHLS and ITHLZ */
           ray2_.ITHLZ = 1;
         }
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighTrans)) == TRUE) {
           ray2_.ITHLS = 2;	/* Signal transparent by setting both ITHLS and ITHLZ */
           ray2_.ITHLZ = 2;
         }
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighPartAtt)) == TRUE) {
           ray2_.ITHLS = 3;}

/* Set which view type */
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewPerspective)) == TRUE) {
           ray2_.ITPPSW = 0;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewPlan)) == TRUE) {
           ray2_.ITPPSW = 1;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewNorthElev)) == TRUE) {
           ray2_.ITPPSW = 2;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewEastElev)) == TRUE) {
           ray2_.ITPPSW = 3;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewFromSun)) == TRUE) {
           ray2_.ITPPSW = 4;}

         gdupdate_();
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_APPLY:
         image_.EYEM[0] = gtk_adjustment_get_value(GTK_ADJUSTMENT (EyePointValueX));
         image_.EYEM[1] = gtk_adjustment_get_value(GTK_ADJUSTMENT (EyePointValueY));
         image_.EYEM[2] = gtk_adjustment_get_value(GTK_ADJUSTMENT (EyePointValueZ));
         image_.VIEWM[0] = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewPointValueX));
         image_.VIEWM[1] = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewPointValueY));
         image_.VIEWM[2] = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewPointValueZ));
         image_.ANG = gtk_adjustment_get_value(GTK_ADJUSTMENT (ViewAngleValue));
         image_.HANG = image_.ANG/2.;
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplayZoneName)) == TRUE) {
           ray2_.ITZNM = 0;} else {ray2_.ITZNM = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySurfaceName)) == TRUE) {
           ray2_.ITSNM = 0;} else {ray2_.ITSNM = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplayVertexNumbers)) == TRUE) {
           ray2_.ITVNO = 0;} else {ray2_.ITVNO = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySurfaceNormals)) == TRUE) {
           ray2_.ITSNR = 0;} else {ray2_.ITSNR = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySiteGrid)) == TRUE) {
           ray2_.ITGRD = 0;} else {ray2_.ITGRD = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (DisplaySiteOrigin)) == TRUE) {
           ray2_.ITORG = 0;} else {ray2_.ITORG = 1;}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (GridSpacingOtimum)) == TRUE) {
           ray2_.GRDIS = 0.;} else {ray2_.GRDIS = gtk_adjustment_get_value(GTK_ADJUSTMENT (GridSpacingDistance));}
         if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (ViewBoundOptimum)) == TRUE) {
           ray2_.ITBND = 1;} else {ray2_.ITBND = 0;}
         /* Remember that C starts at zero, so add one to the zoneid to get the correct value
            for the fortran code.
         */
         gzonpik_.nzg = 0;
         for (izone = 0; izone < c1_.NCOMP; izone++) {
           if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (zone_button[izone])) == TRUE) {
             gzonpik_.nznog[gzonpik_.nzg++] = izone+1;}
         }
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeAll)) == TRUE) {
           ray2_.ITDSP = 0;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeSurfaces)) == TRUE) {
           ray2_.ITDSP = 1;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeExternal)) == TRUE) {
           ray2_.ITDSP = 2;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludePartition)) == TRUE) {
           ray2_.ITDSP = 3;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeSimilar)) == TRUE) {
           ray2_.ITDSP = 4;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeSurObsGrnd)) == TRUE) {
           ray2_.ITDSP = 5;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (IncludeGrnd)) == TRUE) {
           ray2_.ITDSP = 6;}

         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighDefault)) == TRUE) {
           ray2_.ITHLS = 0;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighConstr)) == TRUE) {
           ray2_.ITHLS = 1;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighOpaque)) == TRUE) {
           ray2_.ITHLS = 2;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighTrans)) == TRUE) {
           ray2_.ITHLS = 2;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (HighPartAtt)) == TRUE) {
           ray2_.ITHLS = 3;}

/* Set which view type */
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewPerspective)) == TRUE) {
           ray2_.ITPPSW = 0;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewPlan)) == TRUE) {
           ray2_.ITPPSW = 1;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewNorthElev)) == TRUE) {
           ray2_.ITPPSW = 2;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewEastElev)) == TRUE) {
           ray2_.ITPPSW = 3;}
         if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ViewFromSun)) == TRUE) {
           ray2_.ITPPSW = 4;}

         gdupdate_();
         esru_ask_wire();	/* re-instanciate the help associated with this interface */
         break;
       case GTK_RESPONSE_HELP:

/* need a mechanism for instanciating help array */
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       default:
         no_valid_event = FALSE;
         break;
       }
     }
   gtk_widget_destroy (control);

}

/* ******  Notify level for wireframe button ********** */
void updwire_(avail)
  long int *avail;
{
  wire_avail = (int) *avail;         /* tell the world it is available */
  return;
} /* openwire_ */


/* **** create_static_menus() - create static menus associated with the
 * interface (File, Settings, View, Help etc) */
GtkWidget *create_static_menus( void )
{
   GtkWidget *menu_bar;	/* gtk_hbox to put the files/views/help labels in */
   GtkWidget *file_items;	/* gtk_menu structure for file menu list */
   GtkWidget *settings_items;	/* gtk_menu structure for settings menu list */
   GtkWidget *view_items;	/* gtk_menu structure for view menu list */
   GtkWidget *help_items;	/* gtk_menu structure for help menu list */
   GtkWidget *f_menu_bar;	/* the files (main) menu bar */
   GtkWidget *h_menu_bar;	/* the help menu bar */
   GtkWidget *file_menu;	/* gtk_menu_item_new */
   GtkWidget *settings_menu;	/* gtk_menu_item_new */
   GtkWidget *view_menu;	/* gtk_menu_item_new */
   GtkWidget *help_menu;	/* gtk_menu_item_new */
   GtkWidget *sml_text_fbk;	/* small/medium/large text menu for feedback text */
   GtkWidget *sml_text_gph;	/* small/medium/large text menu for graphic area text */
   GtkWidget *sml_text_mnu;	/* small/medium/large text menu for menu entry text */
   GtkWidget *menu_items;	/* temporary structure for creating new menu entries */
   char buf[128];
   int i;

/* Init static menus (never gtk_show_widget() the menu widget!!  */
   file_items = gtk_menu_new (); /* file_items is the menu structure "File" */

/* Loop that makes temporary menu-entries for "file menu".
 * Create text string to give to gtk_menu_item_new_with_label
 * then use gtk_menu_shell_append to append it to the menu structure
 * and lastly use g_signal_connect_swapped to provide a call-back.
 */
   for (i = 0; i < 3; i++) {
     sprintf (buf, "Test-file - %d", i);
     menu_items = gtk_menu_item_new_with_label (buf);
     gtk_menu_shell_append (GTK_MENU_SHELL (file_items), menu_items);
     g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
		               G_CALLBACK (menuitem_response),
                              (gpointer) g_strdup (buf));
   }

/* Create option menus for font sizes to be used below in settings */
   sml_text_fbk = sml_menu_fbk();
   sml_text_gph = sml_menu_gph();
   sml_text_mnu = sml_menu_mnu();

/* settings_items is the menu structure for "Settings" */
   settings_items = gtk_menu_new ();

/* Use the standard sequence to create the text feedback font entries.
 * and invoke text_feedback_small_font/text_feedback_medium_font etc. if selected. */
   menu_items = gtk_menu_item_new_with_label ("feedback font size");
   gtk_menu_item_set_submenu (GTK_MENU_ITEM (menu_items), sml_text_fbk);
   gtk_menu_shell_append (GTK_MENU_SHELL (settings_items), menu_items);

   menu_items = gtk_menu_item_new_with_label ("graphics font size");
   gtk_menu_item_set_submenu (GTK_MENU_ITEM (menu_items), sml_text_gph);
   gtk_menu_shell_append (GTK_MENU_SHELL (settings_items), menu_items);

   menu_items = gtk_menu_item_new_with_label ("menu font size");
   gtk_menu_item_set_submenu (GTK_MENU_ITEM (menu_items), sml_text_mnu);
   gtk_menu_shell_append (GTK_MENU_SHELL (settings_items), menu_items);

/* setup the views (middle right) static menu */
   view_items = gtk_menu_new ();

/* Use the standard sequence to create the "left" view entry.
 * and invoke esru_rotate_left if selected. */
   menu_items = gtk_menu_item_new_with_label ("rotate left");
   gtk_menu_shell_append (GTK_MENU_SHELL (view_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (esru_rotate_left), NULL);

   menu_items = gtk_menu_item_new_with_label ("rotate right");
   gtk_menu_shell_append (GTK_MENU_SHELL (view_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (esru_rotate_right), NULL);

   menu_items = gtk_menu_item_new_with_label ("rotate up");
   gtk_menu_shell_append (GTK_MENU_SHELL (view_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (esru_elev_up), NULL);

   menu_items = gtk_menu_item_new_with_label ("rotate down");
   gtk_menu_shell_append (GTK_MENU_SHELL (view_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (esru_elev_down), NULL);

/* disable wireframe controls and wireframe toggles
 *  menu_items = gtk_menu_item_new_with_label ("wireframe controls");
 *  gtk_menu_shell_append (GTK_MENU_SHELL (view_items), menu_items);
 *  g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
 *                            G_CALLBACK (esru_wire_pick), NULL);
 *
 *  menu_items = gtk_menu_item_new_with_label ("wireframe toggles");
 *  gtk_menu_shell_append (GTK_MENU_SHELL (view_items), menu_items);
 *  g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
 *                            G_CALLBACK (esru_wire_tog), NULL);
 */

/* proforma for all wireframe controls and toggles */
   menu_items = gtk_menu_item_new_with_label ("wireframe control");
   gtk_menu_shell_append (GTK_MENU_SHELL (view_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (esru_wire_ctl), NULL);


/* setup the help (right) static menu */
   help_items = gtk_menu_new ();

/* Use the standard sequence to create the "about" help entry.
 * and invoke gphelp_disp if selected. */
   menu_items = gtk_menu_item_new_with_label ("About this application");
   gtk_menu_shell_append (GTK_MENU_SHELL (help_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (esru_ask_disp), NULL);

/* Use the standard sequence to create the "On current context" help entry.
 * and invoke gphelp_disp if selected. */
   menu_items = gtk_menu_item_new_with_label ("On current context");
   gtk_menu_shell_append (GTK_MENU_SHELL (help_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (egphelp_disp), NULL);

/* Use the standard sequence to create the "Copyright" help entry.
 * and invoke copyright_disp if selected. */
   menu_items = gtk_menu_item_new_with_label ("Copyright");
   gtk_menu_shell_append (GTK_MENU_SHELL (help_items), menu_items);
   g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
                             G_CALLBACK (copyright_disp), NULL);


/* Then we specify that we want our newly created menues to be the menu
 * bar with labels File/Settings/View/Help */
   file_menu = gtk_menu_item_new_with_label ("File");
   gtk_menu_item_set_submenu (GTK_MENU_ITEM (file_menu), file_items);

   settings_menu = gtk_menu_item_new_with_label ("Preferences");
   gtk_menu_item_set_submenu (GTK_MENU_ITEM (settings_menu), settings_items);

   view_menu = gtk_menu_item_new_with_label ("View");
   gtk_menu_item_set_submenu (GTK_MENU_ITEM (view_menu), view_items);

   help_menu = gtk_menu_item_new_with_label ("Help");
   gtk_menu_item_set_submenu (GTK_MENU_ITEM (help_menu), help_items);

/* Create a new hbox called menu_bar and then associate the menu
 * structures and lists. use gtk_box_pack to position the menus
 * and then finally gtk_menu_shell_append to associate the gtk_menu
 * structure (e.g. file_menu) with the a bar (e.g.f_menu_bar) */
   menu_bar = gtk_hbox_new (FALSE, 0);
   f_menu_bar = gtk_menu_bar_new ();
   h_menu_bar = gtk_menu_bar_new ();
   gtk_box_pack_start (GTK_BOX (menu_bar), f_menu_bar, FALSE, FALSE, 2);
   gtk_box_pack_end (GTK_BOX (menu_bar), h_menu_bar, FALSE, FALSE, 2);
   gtk_menu_shell_append (GTK_MENU_SHELL (f_menu_bar), file_menu);
   gtk_menu_shell_append (GTK_MENU_SHELL (f_menu_bar), settings_menu);
   gtk_menu_shell_append (GTK_MENU_SHELL (f_menu_bar), view_menu);
   gtk_menu_shell_append (GTK_MENU_SHELL (h_menu_bar), help_menu);

   gtk_widget_show_all (menu_bar);	/* display menus */

   return menu_bar;
}

/* create_dialog() - a scrolled dialog area that displays usrmsg text  */
GtkWidget *create_dialog( void )
{
   GtkWidget *scrolled_dialog;
   GtkTextBuffer *buffer;
   PangoFontDescription *pfd;	/* to hold test font */

   dialog = gtk_text_view_new ();

   buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (dialog));
   gtk_text_view_set_editable (GTK_TEXT_VIEW (dialog), FALSE);
   gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (dialog), FALSE);

   scrolled_dialog = gtk_scrolled_window_new (NULL, NULL);
   gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_dialog),
		   	           GTK_POLICY_NEVER, GTK_POLICY_NEVER);

/* react to different sizes of text */
   if (disp_fnt == 0 ) {
     pfd = pango_font_description_from_string("Serif,Medium 8");
     /* fprintf(stderr,"configure font medium 8\n"); debug */
   } else if (disp_fnt == 1 ) {
     pfd = pango_font_description_from_string("Serif,Medium 10");
     /* fprintf(stderr,"configure font medium 10\n"); debug */
   } else if (disp_fnt == 2 ) {
     pfd = pango_font_description_from_string("Serif,Medium 12");
     /* fprintf(stderr,"configure font medium 12\n"); debug */
   } else if (disp_fnt == 3 ) {
     pfd = pango_font_description_from_string("Serif,Medium 14");
     /* fprintf(stderr,"configure font medium 14\n"); debug */
   } else if (disp_fnt == 4 ) {
     pfd = pango_font_description_from_string("Courier,Medium 8");
     /* fprintf(stderr,"configure courier medium 8\n"); debug */
   } else if (disp_fnt == 5 ) {
     pfd = pango_font_description_from_string("Courier,Medium 10");
     /* fprintf(stderr,"configure courier medium 10\n"); debug */
   } else if (disp_fnt == 6 ) {
     pfd = pango_font_description_from_string("Courier,Medium 12");
     /* fprintf(stderr,"configure courier medium 12\n"); debug */
   } else if (disp_fnt == 7 ) {
     pfd = pango_font_description_from_string("Courier,Medium 14");
     /* fprintf(stderr,"configure courier medium 14\n"); debug */
   }
   gtk_widget_modify_font(dialog, pfd);
   pango_font_description_free(pfd);

   gtk_container_add (GTK_CONTAINER (scrolled_dialog), dialog);

   gtk_widget_show_all (scrolled_dialog);

   return scrolled_dialog;
}

/* createwin_ - create the default window set-up. Called from EPAGES in esru_lib.F
   It does initial setup of the application window with graphic feedback, text feedback,
   dialog area and e_menu area (each of these are resizable, but based on parameters
   defined from the calling subroutine.
   Passed in parameters:
   *width - width in pixels
   *height - height in pixels
   *imenuchw - initial menu width (in characters)
   *ilimtty - initial text feedback lines
*/
void createwin_ (long int *width,long int *height,long int *imenuchw,long int *ilimtty,char *head,int lhead)
{
    GtkWidget *frame_u;	/* upper frame for feedback and menu */
    GtkWidget *frame_l;	/* lower frane for dialog and buttons */
    GtkWidget *vpaned_top; /* under everything and assoc with window */
    GtkWidget *vpaneunder; /* under vpaned_top */
    GtkWidget *hpaned;	/* for the feedback and menus assoc with frame_u */
    GtkWidget *vpaned;
    GtkWidget *graphicf;
    GtkWidget *textv;
    GtkWidget *textd;	/* for dialog */
    GtkWidget *textf;
    GtkWidget *feedback;
    GtkWidget *menu_bar;
    int i;
    gchar *head_local;
    PangoContext *context;	/* for use in finding font properties */
    PangoFontMetrics *metrics;
    gint f_height;	/* pixel height of default font */
    gint f_width;	/* pixel width of default font */
    gint iwidth, iheight;	/* local gint values of width and height */
    gint textf_pix_ht;	/* pixel height of text feedback (based on number of lines requested) */
/*     gint menu_pix_wd;	pixel width of initial menu (based on nb of characters *imenuchw) */

/* << Note iappx and iappy from calling subroutine don't seem to be usable
   << as gtk only has limited options to place application on monitor */

/* strings to set interface greys 5 standards and 5 alternatives if allocation fails. */
char *gintstr[] = {
  "grey96","grey94","grey92","grey86","grey64","grey50",
  "grey95","grey93","grey91","grey85","grey63","grey49","grey43" };

    wwc_ok = wwc_macro =0;   /* assume no echo of drawing commands to wwc or macro */
    xold = yold = 0;	/* start eline old points with zero. */
    dash_on = dash_rem = 0;	/* start with no dash offsets */
    dbx1_avail = 0;	/* no graphics assumed at start */
    c3dcl = c3dcr = c3dct = c3dcb = 0; /* dbx1 char offsets left, right, top, bottom */
    ter = 8;            /* terminal type passed on initial call (set initial
                            value to 8 which will be overwritten if run in
                            text mode - ie. text mode fail safe for any
                            system execution calls.  */
    child_ter = 8;      /* child process terminal type assumed to be graphic if
                          * this function has been called. */
    help_width = 0;	/* current help max line length */
    help_lines = 0;	/* current number of active help lines */

/* initial clear of popup menu lines list and popup item type string */
    for ( i = 0; i < MENU_LIST_LEN-1; i++ ) {
      strcpy(pm_list[i],
      "                                                                                   ");
    }
    strcpy(pmtype_list,"                                           ");
    pm_width = 0;	/* set initial popup char width */
    pm_lines = 0;	/* set initial popup items */
    wire_avail = 0;      /* wireframe adjustment initial state */

/* Initial values for information to pass back to fortran related to number of colours */
    ncscale = 0; /* number of assigned colours in colour scale */
    ngscale = 0; /* number of assigned colours in grey scale */
    ngr = 0; /* number of assigned interface colours */
    izc = 0; /* number of assigned zone colours */
    mdepth = 0;  /* to pass to fortran: sceen depth */

    gtk_init (NULL, NULL);

    menuchw = *imenuchw;	/* remember initial request */
    fprintf(stderr,"WIN SIZE MENUCHW LIMTTY %ld %ld %ld %ld\n", *height,*imenuchw,*ilimtty,menuchw);	/* debug */
    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);	/* create top level window structure */
    head_local = g_strndup(head, (gsize) lhead);
    gtk_window_set_title (GTK_WINDOW (window), head_local);

    /*
      Set signal handlers.
    */
    g_signal_connect (G_OBJECT (window), "delete_event",
		      G_CALLBACK (delete_event), NULL);
    g_signal_connect (G_OBJECT (window), "destroy",
	              G_CALLBACK (destroy), NULL);

    gtk_container_set_border_width (GTK_CONTAINER (window), 1);
    iwidth = (gint) *width; iheight = (gint) *height;
    gtk_window_set_default_size (GTK_WINDOW (window), iwidth, iheight);
    xrt_width = iwidth; xrt_height = iheight;  /* remember initial application size */

/* centre application on monitor (gtk does not support specific pixel placement) */
    gtk_window_set_position ( GTK_WINDOW (window),GTK_WIN_POS_CENTER);

/*
  In the application window create a menu bar, an upper frame_u (for feedback and
  menu and a lower frame_l for dialog and control buttons.

  Create vpaned_top as a (vertical) box at the top of the application and the first
  item to be added to this container is the menu_bar.
*/
    vpaned_top = gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (window), vpaned_top);
    gtk_widget_show (vpaned_top);

/* Creat the menu bar across the top */
    menu_bar = create_static_menus ();
    gtk_box_pack_start (GTK_BOX (vpaned_top), menu_bar, FALSE, FALSE, 0);
    gtk_widget_show (menu_bar);

/*  Create a vpaneunder widget to go below the menu_bar to hold all the
    others so that there can be an upper and lower frame to work within.
*/
    vpaneunder = gtk_vpaned_new ();
    gtk_box_pack_start (GTK_BOX (vpaned_top), vpaneunder, TRUE, TRUE, 5);
    gtk_widget_show (vpaneunder);

/* Using vpaneunder as a typical widget, get its context and the current
   (probably default) font metrics. Based on code in gtk/gtkmenuitem.c
   function get_minimum_width.
*/
    context = gtk_widget_get_pango_context (vpaneunder);
    metrics = pango_context_get_metrics (context, vpaneunder->style->font_desc,
	      pango_context_get_language (context));
    f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
               pango_font_metrics_get_descent (metrics));
    f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (metrics));
    pango_font_metrics_unref (metrics);
    fprintf(stderr,"font height and width is %d %d\n", f_height,f_width); /* debug */

/* Create upper frame, just below the menu bar and within vpaneunder */
    frame_u = gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (GTK_FRAME (frame_u), GTK_SHADOW_OUT);
    gtk_paned_pack1 (GTK_PANED (vpaneunder), frame_u, TRUE, TRUE);
    gtk_widget_show (frame_u);

/* Create lower frame and request its size be sufficient for 2 lines of dialog */
    frame_l = gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (GTK_FRAME (frame_l), GTK_SHADOW_OUT);
    gtk_widget_set_size_request (frame_l, -1, (2 * f_height +2)); /* 2 lines of text plus a bit */
    gtk_paned_pack2 (GTK_PANED (vpaneunder), frame_l, FALSE, TRUE);
    gtk_widget_show (frame_l);
    textd = create_dialog ();
    gtk_container_add (GTK_CONTAINER (frame_l), textd);
    gtk_widget_show (textd);

/* Create a hpaned widget and add it to our frame_u.
   hpaned holds the feedback panes as well as the dynamic and static menus.
*/
    hpaned = gtk_hpaned_new ();
    gtk_container_add (GTK_CONTAINER (frame_u), hpaned);
    gtk_widget_show (hpaned);

    feedback = gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (GTK_FRAME (feedback), GTK_SHADOW_OUT);
    gtk_paned_pack1 (GTK_PANED (hpaned), feedback, TRUE, TRUE);
    gtk_widget_show (feedback);

/* Setup frame for the menu and request initial width based on *imenuchw
   Remember menu_pix_wd for use elsewhere. NOTE: find a way to update
   if the application is re-sized */
    emenu = gtk_frame_new (NULL);
    menu_pix_wd = (gint) *imenuchw * f_width;
    fprintf(stderr,"menu chars and pix wd %ld %d\n", *imenuchw,menu_pix_wd); /* debug */

/* The reserved area for the graphic feedback is different for each application.
 * The following sizes are based on the standard iappw values defined in each
 * modules setup (and does not yet take into account that the user might have
 * asked for a smaller initial size).
 */
    if(strncmp(cappl, "aco", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 560);
    } else if(strncmp(cappl, "cfg", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 450);
    } else if(strncmp(cappl, "clm", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 470);
    } else if(strncmp(cappl, "bld", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 450);
    } else if(strncmp(cappl, "bps", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 450);
    } else if(strncmp(cappl, "dfs", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 450);
    } else if(strncmp(cappl, "e2r", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 440);
    } else if(strncmp(cappl, "eco", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 550);
    } else if(strncmp(cappl, "grd", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 440);
    } else if(strncmp(cappl, "ish", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 440);
    } else if(strncmp(cappl, "mfs", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 440);
    } else if(strncmp(cappl, "mld", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 440);
    } else if(strncmp(cappl, "mrt", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 440);
    } else if(strncmp(cappl, "net", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 550);
    } else if(strncmp(cappl, "pdb", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 400);
    } else if(strncmp(cappl, "plt", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 500);
    } else if(strncmp(cappl, "prj", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 520);
    } else if(strncmp(cappl, "pro", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 440);
    } else if(strncmp(cappl, "res", 3) == 0) {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 520);
    } else {
      gtk_widget_set_size_request (emenu, menu_pix_wd, 500);
    }

    gtk_paned_pack2 (GTK_PANED (hpaned), emenu, FALSE, TRUE);
    gtk_widget_show (emenu);

/* so the emenu is a frame which we could try
   using the esp_list_in_frame function with */

    /*
       Create a vpaned widget and add it to our feedback pane window
    */
    vpaned = gtk_vpaned_new ();
    gtk_container_add (GTK_CONTAINER (feedback), vpaned);
    gtk_widget_show (vpaned);

/*  Now create the contents of the two halves of the window. */
    graphicf = gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (GTK_FRAME (graphicf), GTK_SHADOW_OUT);
    gtk_paned_pack1 (GTK_PANED (vpaned), graphicf, TRUE, TRUE);

    graphic = gtk_drawing_area_new ();
    gtk_container_add (GTK_CONTAINER (graphicf), graphic);
    gtk_widget_show_all (graphicf);

/* Set up signal handlers for the graphics area here. */
    g_signal_connect (G_OBJECT (graphic), "expose_event",
                      G_CALLBACK (expose_event), NULL);
/*    g_signal_connect (G_OBJECT (graphic), "configure_event",
                      G_CALLBACK (configure_event), NULL); */
    gtk_widget_set_events (graphic, GDK_EXPOSURE_MASK
			          | GDK_LEAVE_NOTIFY_MASK
			          | GDK_BUTTON_PRESS_MASK
			          | GDK_POINTER_MOTION_MASK
			          | GDK_POINTER_MOTION_HINT_MASK);

/* Create frame for text feedback. Size it based on number of lines requested (ilimtty) */
    textf = gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (GTK_FRAME (textf), GTK_SHADOW_OUT);
    textf_pix_ht = (gint) *ilimtty * f_height +2;
    fprintf(stderr,"text feedback lines and pix ht %ld %d\n", *ilimtty,textf_pix_ht); /* debug */
    gtk_widget_set_size_request (textf, -1, textf_pix_ht);
    gtk_paned_pack2 (GTK_PANED (vpaned), textf, FALSE, TRUE);
    textv = create_text ();
    gtk_container_add (GTK_CONTAINER (textf), textv);
    gtk_widget_show (textf);

    gtk_widget_show (window);

    menu_loop = g_main_loop_new (NULL, FALSE);

/* Get the system color map */
    cmap = gdk_colormap_get_system ();

/* Get the context to use for text rendering in the graphic window.
   Guess the default language is English. */
    pango_context = NULL;
    pango_context = gtk_widget_get_pango_context(graphic);
    pango_context_set_language (pango_context, pango_language_from_string ("en_US"));
    pango_metrics = pango_context_get_metrics (pango_context, graphic->style->font_desc,
	      pango_context_get_language (pango_context));
    f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics) +
               pango_font_metrics_get_descent (pango_metrics));
    f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (pango_metrics));
/*    pango_font_metrics_unref (pango_metrics); this should be placed where the application exits */
    fprintf(stderr,"pango en_US font height and width is %d %d\n", f_height,f_width); /* debug */

/* Set up a range of interface and black and white colors */
/* << why not use quoted string rather than string variable? >> */
    if(gdk_color_parse("white",&white)==1) {	/* return color white based on standard rgb.txt string */
      if (gdk_color_alloc(cmap, &white)!=1) {
        g_error ("couldn't allocate white");	/* warn if colour cannot be allocated */
      }
    }
    if(gdk_color_parse("black",&black)==1) {	/* return color black based on standard rgb.txt string */
      if (gdk_color_alloc (cmap, &black)!=1) {
        g_error ("couldn't allocate black");	/* warn if colour cannot be allocated */
      }
    }
    if(gdk_color_parse("#B2C0DC",&bg)==1) {	/* return color for background based XV approach 198|213|226 */
      if (gdk_color_alloc (cmap, &bg)!=1) {
        g_error ("couldn't allocate bg");	/* warn if colour cannot be allocated */
      }
    }

/* Get the system color map, the graphic background pixmap (the one that is drawn to
   prior top copying to the graphic portion of the interface. */
    cmap = gdk_colormap_get_system ();

    gr_image = NULL;
    gr_image_old = NULL;
    gr_image = gdk_pixmap_new(graphic->window,
       graphic->allocation.width,graphic->allocation.height,-1);

    mdepth = gdk_drawable_get_depth(gr_image); /* recover depth of the display */

/* generate a gdk graphic context for the graphic window */
    gc = gdk_gc_new (graphic->window);

/* clear graphic window by drawing whole area with white then reset foreground to black */
    gdk_gc_set_foreground(gc,&white);
    gdk_draw_rectangle (gr_image,gc,TRUE,0, 0,
	graphic->allocation.width,graphic->allocation.height);

    gtk_widget_queue_draw_area (graphic,0, 0,
	graphic->allocation.width,graphic->allocation.height);  /* force queued actions to be updated */

/* esru_x interface grey colors (probably can be depreciated) The logic is that there are a pair of similar
   colours to try from the gintstr array. If the first cannot be allocated the second one is tried. The
   call to alloc_color has the parameter for best-fit set to true to limit the chance of failure. This
   follows the pattern set in esru_x.c. */
    if ( mdepth > 1 ) {   /* only if a reasonable display */
      if ((gdk_color_parse(gintstr[0],&gmenuhl)==1) && (gdk_colormap_alloc_color(cmap,&gmenuhl,FALSE,TRUE)==1)) {
        ngr=ngr+1;
      } else {
        fprintf(stderr,"Problem colour %s\n",gintstr[0]);
        if ((gdk_color_parse(gintstr[6],&gmenuhl)==1) && (gdk_colormap_alloc_color(cmap,&gmenuhl,FALSE,TRUE)==1)) ngr=ngr+1;
      }
      if ((gdk_color_parse(gintstr[1],&gmodbg)==1) && (gdk_colormap_alloc_color(cmap,&gmodbg,FALSE,TRUE)==1)) {
        ngr=ngr+1;
      } else {
        fprintf(stderr,"Problem colour %s reset to %s\n",gintstr[1],gintstr[7]);
        if ((gdk_color_parse(gintstr[7],&gmodbg)==1) && (gdk_colormap_alloc_color(cmap,&gmodbg,FALSE,TRUE)==1)) ngr=ngr+1;
      }
      if ((gdk_color_parse(gintstr[2],&gpopfr)==1) && (gdk_colormap_alloc_color(cmap,&gpopfr,FALSE,TRUE)==1)) {
        ngr=ngr+1;
      } else {
        fprintf(stderr,"Problem colour %s reset to %s\n",gintstr[2],gintstr[8]);
        if ((gdk_color_parse(gintstr[8],&gpopfr)==1) && (gdk_colormap_alloc_color(cmap,&gpopfr,FALSE,TRUE)==1))  ngr=ngr+1;
      }
      if ((gdk_color_parse(gintstr[3],&gfeedfr)==1) && (gdk_colormap_alloc_color(cmap,&gfeedfr,FALSE,TRUE)==1)) {
        ngr=ngr+1;
      } else {
        fprintf(stderr,"Problem colour %s reset to %s\n",gintstr[3],gintstr[9]);
        if ((gdk_color_parse(gintstr[9],&gfeedfr)==1) && (gdk_colormap_alloc_color(cmap,&gfeedfr,FALSE,TRUE)==1))  ngr=ngr+1;
      }
      if ((gdk_color_parse(gintstr[4],&ginvert)==1) && (gdk_colormap_alloc_color(cmap,&ginvert,FALSE,TRUE)==1)) {
        ngr=ngr+1;
      } else {
        fprintf(stderr,"Problem colour %s reset to %s\n",gintstr[4],gintstr[10]);
        if ((gdk_color_parse(gintstr[10],&ginvert)==1) && (gdk_colormap_alloc_color(cmap,&ginvert,FALSE,TRUE)==1))  ngr=ngr+1;
      }
      if ((gdk_color_parse(gintstr[5],&grey50)==1) && (gdk_colormap_alloc_color(cmap,&grey50,FALSE,TRUE)==1)) {
        ngr=ngr+1;
      } else {
        fprintf(stderr,"Problem colour %s reset to %s\n",gintstr[5],gintstr[11]);
        if ((gdk_color_parse(gintstr[11],&grey50)==1) && (gdk_colormap_alloc_color(cmap,&grey50,FALSE,TRUE)==1))  ngr=ngr+1;
      }
      if ((gdk_color_parse(gintstr[12],&grey43)==1) && (gdk_colormap_alloc_color(cmap,&grey43,FALSE,TRUE)==1)) {
        ngr=ngr+1;
      } else {
        fprintf(stderr,"Problem colour %s reset to %s\n",gintstr[12],gintstr[11]);
        if ((gdk_color_parse(gintstr[11],&grey43)==1) && (gdk_colormap_alloc_color(cmap,&grey43,FALSE,TRUE)==1))  ngr=ngr+1;
      }
      ngr=ngr+2;	/* include black (ngr-1) and white (ngr) */
      fprintf(stderr,"greys ngr %ld\n",ngr);
      infofg = black;  infobg = white;

    } else {
       fprintf(stderr,"Display does not support many colours\n");
    }
  g_free (head_local);
  
  calculate_font_metrics();  /* get the width and height of all font combinations */
 
   
    }

/* The colour routines taken from esru_x.c and converted to gtk/gdk */

/* ********* setcscale_() colour scale (50 or 25 steps) ******* */
/* Called with no parameters, sets global variable ncscale. The local
   variable cscaleok is not used (yet). */
void setcscale_() {

/* Color named arrays taken from esru_x.c */
  char *cscalestr[] = { /* colour scale RGB HEX values (to represent 49 steps of temperature) */
    "#FF0000","#FF1500","#FF2B00","#FF4000","#FF5500","#FF6A00","#FF8000","#FF9500","#FFAA00","#FFBF00",
    "#FFD500","#FFEA00","#FFFF00","#EAFF00","#D5FF00","#BFFF00","#AAFF00","#95FF00","#80FF00","#6AFF00",
    "#55FF00","#40FF00","#2AFF00","#15FF00","#00FF00","#00FF15","#00FF2B","#00FF40","#00FF55","#00FF6A",
    "#00FF80","#00FF95","#00FFAA","#00FFBF","#00FFD4","#00FFEA","#00FFFF","#00EAFF","#00D4FF","#00BFFF",
    "#00AAFF","#00AAFF","#00AAFF","#006AFF","#0055FF","#0040FF","#002BFF","#0015FF","#0000FF" };
  gint ic,ih;
  gboolean cscaleok[49];
/* assign colour scale to cscale array. */
  for (ic=0; ic<49; ic++) {
    if (gdk_color_parse(cscalestr[ic],&cscale[ic])==1) {
      if (gdk_colormap_alloc_color(cmap,&cscale[ic],FALSE,TRUE)==1) {
        ncscale=ncscale+1;
        cscaleok[ic] = 1;
      } else {
        fprintf(stderr,"Unable to create colour %s\n",cscalestr[ic]);
        cscaleok[ic] = 0;
      }
    } else {
      fprintf(stderr,"Unable to parse colour %s\n",cscalestr[ic]);
      cscaleok[ic] = 0;
    }
  }
/* Some colours not allocated attempt half of the colours. Begin by freeing initial allocated set. */
  if ( ncscale <= 45 ) {
    gdk_colormap_free_colors(cmap,&cscale[0],(gint) ncscale);
    fprintf(stderr,"Trying reduced colour set\n");
    ncscale = 0;
    ih = -1;
    for (ic=0; ic<24; ic++) {
      ih = ih + 2;
      if (gdk_color_parse(cscalestr[ih],&cscale[ic])==1) {
        if (gdk_colormap_alloc_color(cmap,&cscale[ic],FALSE,TRUE)==1) {
          ncscale=ncscale+1;
          cscaleok[ic] = 1;
        } else {
          fprintf(stderr,"Unable to create colour %s\n",cscalestr[ic]);
          cscaleok[ic] = 0;
        }
      } else {
        fprintf(stderr,"Unable to parse colour %s\n",cscalestr[ic]);
        cscaleok[ic] = 0;
      }
    }
  }
  fprintf(stderr,"Created cscale %ld\n",ncscale);	/* debug statement */
  return;
}

/* ********* clrcscale_() clear colour scale (50 or 25 steps) ******* */
/* Frees ncscale colours and then resets global ncscale */
void clrcscale_() {
  gdk_colormap_free_colors(cmap,&cscale[0],(gint) ncscale);
  ncscale = 0;
  fprintf(stderr,"Freed cscale colour set\n");	/* debug statement */
  return;
}

/* ********* setgscale_() setgscale_() grey scale (49 or 24 steps) ******* */
/* Called with no parameters, sets global variable ngscale. The local
   variable gscaleok is not used (yet). */
void setgscale_() {

/* Color named arrays taken from esru_x.c */
  char *gscalestr[] = {
  "grey97","grey95","grey93","grey91","grey89","grey87","grey85","grey83","grey81",
  "grey79","grey77","grey75","grey73","grey71","grey69","grey67","grey65","grey63","grey61",
  "grey59","grey57","grey55","grey53","grey51","grey49","grey47","grey45","grey43","grey41",
  "grey39","grey37","grey35","grey33","grey31","grey29","grey27","grey25","grey23","grey21",
  "grey19","grey17","grey15","grey13","grey11","grey9","grey7","grey5","grey3","grey1"};
  gint ic,ih;
  gboolean gscaleok[49];
/* assign grey scale to gscale array. */
  for (ic=0; ic<48; ic++) {
    if (gdk_color_parse(gscalestr[ic],&gscale[ic])==1) {
      if (gdk_colormap_alloc_color(cmap,&gscale[ic],FALSE,TRUE)==1) {
        ngscale=ngscale+1;
        gscaleok[ic] = 1;
      } else {
        fprintf(stderr,"Unable to create colour %s\n",gscalestr[ic]);
        gscaleok[ic] = 0;
      }
    } else {
      fprintf(stderr,"Unable to parse colour %s\n",gscalestr[ic]);
      gscaleok[ic] = 0;
    }
  }
/* Some colours not allocated attempt half of the colours. Begin by freeing initial allocated set. */
  if ( ngscale <= 47 ) {
    gdk_colormap_free_colors(cmap,&gscale[0],(gint) ngscale);
    fprintf(stderr,"Trying reduced grey set\n");
    ngscale = 0;
    ih = -1;
    for (ic=0; ic<24; ic++) {
      ih = ih + 2;
      if (gdk_color_parse(gscalestr[ih],&gscale[ic])==1) {
        if (gdk_colormap_alloc_color(cmap,&gscale[ic],FALSE,TRUE)==1) {
          ngscale=ngscale+1;
          gscaleok[ic] = 1;
        } else {
          fprintf(stderr,"Unable to create colour %s\n",gscalestr[ic]);
          gscaleok[ic] = 0;
        }
      } else {
        fprintf(stderr,"Unable to parse colour %s\n",gscalestr[ic]);
        gscaleok[ic] = 0;
      }
    }
  }
  fprintf(stderr,"Created grey cscale %ld\n",ngscale);
  return;
}

/* ********* clrgscale_() clear grey scale ******* */
/* Frees ngscale colours and then resets global ngscale */
void clrgscale_() {
  gdk_colormap_free_colors(cmap,&gscale[0],(gint) ngscale);
  ngscale = 0;
  fprintf(stderr,"Freed gscale colour set\n");
  return;
}

/* ********* setzscale_() zone colour scale (100 steps) ******* */
/* Called with no parameters, sets global variable izc. The local
   variable zscaleok is not used (yet). */
void setzscale_() {

/* Color named arrays taken from esru_x.c (duplicated after 1st 50 colours */
  char *zscalestr[] = { /* colour names from rgb.txt to represent zone colours */
    "red","MidnightBlue","peru","ForestGreen","khaki","grey14","turquoise","magenta","gold4","firebrick",
    "DarkCyan","khaki3","grey25","RoyalBlue","tomato","grey34","OliveDrab","PaleGreen","orange","grey40",
    "coral2","tan4","SeaGreen","grey60","maroon4","gold3","grey46","PowderBlue","sienna","azure4","grey20","burlywood2",
    "grey50","khaki2","NavyBlue","sienna3","DarkGreen","gold","magenta3","grey80","turquoise2","gold1","tomato3",
    "grey70","orange3","grey37","maroon1","grey19","tan2","green3",
    "red","MidnightBlue","peru","ForestGreen","khaki","grey14","turquoise","magenta","gold4","firebrick",
    "DarkCyan","khaki3","grey25","RoyalBlue","tomato","grey34","OliveDrab","PaleGreen","orange","grey40",
    "coral2","tan4","SeaGreen","grey60","maroon4","gold3","grey46","PowderBlue","sienna","azure4","grey20","burlywood2",
    "grey50","khaki2","NavyBlue","sienna3","DarkGreen","gold","magenta3","grey80","turquoise2","gold1","tomato3",
    "grey70","orange3","grey37","maroon1","grey19","tan2","green3" };
  gint ic;
  gboolean zscaleok[100];
/* assign zone scale to gscale array. */
  for (ic=0; ic<99; ic++) {
    if (gdk_color_parse(zscalestr[ic],&zscale[ic])==1) {
      if (gdk_colormap_alloc_color(cmap,&zscale[ic],FALSE,TRUE)==1) {
        izc = izc + 1;
        zscaleok[ic] = 1;
      } else {
        fprintf(stderr,"Unable to create colour %s\n",zscalestr[ic]);
        zscaleok[ic] = 0;
      }
    } else {
      fprintf(stderr,"Unable to parse colour %s\n",zscalestr[ic]);
      zscaleok[ic] = 0;
    }
  }
  return;
}

/* *** foundcolour_() notify fortran of monitor depth and nb interface greys, steps
   in colour scale, greyscale and zone scale */
void foundcolour_(md,nic,ncs,ngs,nzc)
  long int *md,*nic,*ncs,*ngs,*nzc;
{
  *md = mdepth;
  *nic = ngr;
  *ncs = ncscale;
  *ngs = ngscale;
  *nzc = izc;
  return;
}

/* ********** winscl_() Set forground colour ************* */
/* action "g" grey scale index 0 >= n <= ngscale */
/* action "z" zone index 0 >= n <= izc */
/* action "c" colour scale index  0 >= n <= ncscale */
/* action "i" interface greys index  0 >= n <= ngr */
/* action "-" resets the forground to standard fg colour */
void winscl_(act,n)
char *act;  /* single character passed */
long int *n;
{
 int ic;
 ic = (gint) *n;
/* sets the current forground colour n depending on which active colour set being used */
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
  return;
}

/* **************  Erase whole screen. ************* */
/* << this needs testing and might not be necessary >> */
/* Logic is to free the pixmap entire_image if it exists and then
   create a pixmap with the extents of the current overall application
   window, set the foreground colour to white, write white to
   the pixmap and then put the pixmap onto the window widget.
   Just prior to returning the fg colour is reset to black.
*/
void winclr_()
{
  fprintf(stderr,"call to clear entire application display (ignored)\n");

/* guts commented out until we see if there is an actual need for this...
 if (entire_image) {
    g_object_unref(entire_image);
  }

  entire_image = gdk_pixmap_new(window->window,
	window->allocation.width,window->allocation.height,-1);
  gdk_gc_set_foreground(gc,&white);
  gdk_draw_rectangle (entire_image,gc,TRUE,0, 0,
	window->allocation.width,window->allocation.height);

  fprintf(stderr,"about to draw pixmap to clear application \n");
  gdk_draw_drawable(window->window, gc, entire_image,0, 0, 0, 0,
	window->allocation.width,window->allocation.height);
  gdk_gc_set_foreground(gc,&black);
*/
  return;
}

/* **************  Open a 3D viewing box *************** */
/*
 Passed the character width of the main control menu (not used), the number of
 dialog lines (not used), the width of
 the left, right inside margins in terms of number of characters
 with the default font and the top and bottom inside margins in terms
 of lines of characters.

 Returns the pixel coord of the viewing box left, right, top, bottom as well
 as its overall pixel width & height.
 dbx1 is the outer box (including axes) and viewbx is the image area.

 This should be called after epages so that the context is already established.
*/
void win3d_(menu_char,cl,cr,ct,cb,vl,vr,vt,vb,gw,gh)
 long int	*menu_char,*gw,*gh;
 long int	*cl,*cr,*ct,*cb;
 long int	*vl,*vr,*vt,*vb;
{
 PangoFontDescription *pfd;	/* to hold test font */
 PangoContext *context;	/* for use in finding font properties */
 PangoFontMetrics *metrics;
 gint label_ht,mf_width;     /* box label height and width of menu characters */
 gint g_width,g_height;	/* size of the graphics widget */
 gint b_top, b_bottom, b_left, b_right; /* pixels at top/bottom/left/right */
 gint f_height, f_width; /* height and width of current font */

 dbx1_avail = 1;        /* tell world that graphic box exists */
 c3dcl = (gint) *cl; c3dcr = (gint) *cr;   /* remember character offsets    */
 c3dct = (gint) *ct; c3dcb = (gint) *cb;

/* create font description to use for resetting the graphic font.
 * Use pango_font_get_metrics to get font width and height. */
 if (butn_fnt == 0 ) {
   pfd = pango_font_description_from_string("Serif,Medium 8");
   /* fprintf(stderr,"refresh graphic font medium 8\n"); debug */
 } else if (butn_fnt == 1 ) {
   pfd = pango_font_description_from_string("Serif,Medium 10");
   /* fprintf(stderr,"refresh graphic font medium 10\n"); debug */
 } else if (butn_fnt == 2 ) {
   pfd = pango_font_description_from_string("Serif,Medium 12");
   /* fprintf(stderr,"refresh graphic font medium 12\n"); debug */
 }
 gtk_widget_modify_font(graphic, pfd);	/* << ?? >> */
 context = gtk_widget_get_pango_context (graphic);
 metrics = pango_context_get_metrics (context, pfd,
           pango_context_get_language (context));
 f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
            pango_font_metrics_get_descent (metrics));
 f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (metrics));
 pango_font_metrics_unref (metrics);
 pango_font_description_free(pfd);
 label_ht = f_height+6;
 mf_width = f_width;	/* menu font to get right side of box.  */

/* << need this information to include in call to updview_ */
/* find the size of the graphic window. Note: because all drawing tasks
 * are to the pixmap which is the same size as graphic, the coordinates
 * derived are based on upper left of the pixmap = 0, 0. */
 gdk_drawable_get_size(graphic->window,&g_width,&g_height);
 fprintf(stderr,"win3d font height width is %d %d gr_w %d gr_h %d\n", f_height,f_width,g_width,g_height); /* debug */
 b_top = 0 + (f_height * (*ct));
 b_bottom = g_height - 9 - (f_height * ((gint) *cb));
 b_left = 0 + 9 + (f_width * ((gint) *cl));
 b_right = g_width - (f_width * ((gint) *cr));
 *vl=b_left;                   /* pixel @ left   */
 *vr=b_right;                  /* pixel @ right  */
 *vt=b_top;                    /* pixel @ top    */
 *vb=b_bottom;                 /* pixel @ bottom */
 *gh=b_bottom - b_top;  /* box within axis pixel height  */
 *gw=b_right - b_left;  /* box within axis pixel width */

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*win3d\n");
   fprintf(wwc,"%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld\n",
		*menu_char,*cl,*cr,*ct,*cb,*vl,*vr,*vt,*vb,*gw,*gh);
 }
  fprintf(stderr,"win3d mcw %ld cl %ld cr %ld ct %ld cb %ld vl %ld vr %ld vt %ld vb %ld gbw %ld gbh %ld\n",
		*menu_char,*cl,*cr,*ct,*cb,*vl,*vr,*vt,*vb,*gw,*gh); /* debug */
  return;
} /* win3d_ */

/* ************* startbuffer_() create a new graphic buffer pixmap *** */
/* graphic is the gtk widget for the graphic feedback area. This
   creates a new bitmap the same size (and paints it white and then
   resets the foreground colour to black before returning).
   Note: this should be called once to clear pixmap before the drawing primitives.
   But if it is not called and there is a previous pixmap drawing primitives will
   be appended to the pixmap. */
void startbuffer_()
{
  fprintf(stderr,"new pixmap for graphic area %d %d \n",graphic->allocation.width,graphic->allocation.height); /* debug */
  if (gr_image) {
    g_object_unref(gr_image);
    fprintf(stderr,"unreferenced the pixmap\n"); /* debug */
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
  return;
}

/* ************* forceflush_() force graphic buffer to flush ********* */
/* graphic is the gtk widget for the graphic feedback area. This
   copies the current contents of gr_image into graphic. */
void forceflush_()
{
/*  fprintf(stderr,"flush pixmap to graphic area %d %d \n",graphic->allocation.width,graphic->allocation.height);  debug */
  gdk_draw_drawable(graphic->window, gc, gr_image,0, 0, 0, 0,
	graphic->allocation.width,graphic->allocation.height);
  return;
}

