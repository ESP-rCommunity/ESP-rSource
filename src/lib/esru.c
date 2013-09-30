/* Miscel non-graphic functions taken from esru_x.c
   
   winfnt(n)   changes the font (4 different sizes 0,1,2,3)
   Timer(msec) pause_for_milliseconds
   pausems_() pause_for_milliseconds
   pauses_() pause_for_seconds
   curmodule_() pass in info on the current application from fortran
   userfonts() set fonts for common display tasks
*/
   
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/time.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include "esp-r.h"
#include <commons.h>


/* circa Dec. 2005, MinGW apparently no longer offers 'sleep'.
   Alias to MS windows Sleep, which takes milliseconds 
   instead of seconds */
#ifdef MINGW
#include <windows.h>
#define sleep(s) Sleep(s*1000)
#endif 

extern FILE *wwc;
extern gint f_height;
extern gint f_width;

/* ************ Select a font **************** */
/* select one of the 8 fonts by its index, load it and update the graphic context.
 * for some reason font_0 has been cleared after initial jwinint call and so
 * it is refreshed. 
 * Displays the vetrices in the smallest font avaialble until more fonts are added
 * Surface normals will be displayed in the font selected by user 
 */
void winfnt_(font_index)
 long int *font_index;

{
   
   gint ifont_index;
   PangoFontDescription *pfd;	/* to hold test font */
   long int ifsc,itfsc,imfsc,lttyc; /* parameters must be long int */

   ifont_index = (gint) *font_index;

/* Originally there were three options for fonts and disp_fnt was either 0, 1, or 2.
 * With the addition of Serif Largest, Courier Small, Courier Medium, Courier Large and Courier Largest,
 * the font_index also has the values 3, 4, 5, 6, 7 
 * << NOTE: the fortran side does not know about these new values >>
 */


  if (ifont_index == 0 ) {
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 8");
     f_height = font_calculations_array[courier_small].f_height;
     f_width  = font_calculations_array[courier_small].f_width;
#else
     pfd = pango_font_description_from_string("Serif,Medium 8");
     f_height = font_calculations_array[serif_small].f_height;   // pre-calculated value of f_height is read from the array
     f_width  = font_calculations_array[serif_small].f_width;    // pre-calculated value of f_width  is read from the array
#endif 
   //fprintf(stderr,"graphic_reset at serif medium 8 change font height and width is %d %d %d\n", f_height, f_width, ifont_index);	//debug
    
  } else if (ifont_index == 1 ) {
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 10");
     f_height = font_calculations_array[courier_medium].f_height;
     f_width  = font_calculations_array[courier_medium].f_width;
#else
     pfd = pango_font_description_from_string("Serif,Medium 10");
     f_height = font_calculations_array[serif_medium].f_height;
     f_width  = font_calculations_array[serif_medium].f_width;
#endif
   //fprintf(stderr,"graphic_reset at serif medium 10 change font height and width is %d %d %d\n", f_height, f_width,ifont_index );	//debug
      
   } else if (ifont_index == 2 ) {
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 12");
     f_height = font_calculations_array[courier_large].f_height;
     f_width  = font_calculations_array[courier_large].f_width;
#else
     pfd = pango_font_description_from_string("Serif,Medium 12");
     f_height = font_calculations_array[serif_large].f_height;
     f_width  = font_calculations_array[serif_large].f_width;
#endif
   // fprintf(stderr,"graphic_reset at serif medium 12 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	

   } else if (ifont_index == 3 ) {
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 14");
     f_height = font_calculations_array[courier_largest].f_height;
     f_width  = font_calculations_array[courier_largest].f_width;
#else
     pfd = pango_font_description_from_string("Serif,Medium 14");
     f_height = font_calculations_array[serif_largest].f_height;
     f_width  = font_calculations_array[serif_largest].f_width;
#endif 
   //fprintf(stderr,"graphic_reset at serif medium 14 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	
   }
   
   else if (ifont_index == 4) {
   pfd = pango_font_description_from_string("Courier,Medium 8");
   f_height = font_calculations_array[courier_small].f_height;
   f_width  = font_calculations_array[courier_small].f_width;
 
   //fprintf(stderr,"graphic_reset at courier medium 8 change font height and width is %d %d %d \n ", f_height, f_width, ifont_index);	//debug	 
      
   } 
   else if (ifont_index == 5  ) {
   pfd = pango_font_description_from_string("Courier,Medium 10");
   f_height = font_calculations_array[courier_medium].f_height;
   f_width  = font_calculations_array[courier_medium].f_width;
 
   // fprintf(stderr,"graphic_reset at courier medium 10 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	 
   }
   else if (ifont_index == 6 ) {
   pfd = pango_font_description_from_string("Courier,Medium 12");
   f_height = font_calculations_array[courier_large].f_height;
   f_width  = font_calculations_array[courier_large].f_width;
 
     //fprintf(stderr,"graphic_reset at courier medium 12 change font height and width is %d %d %d\n", f_height, f_width, ifont_index);	//debug	  
   }
   else if (ifont_index == 7 ) {
   pfd = pango_font_description_from_string("Courier,Medium 14");
   f_height = font_calculations_array[courier_largest].f_height;
   f_width  = font_calculations_array[courier_largest].f_width;
 
   //fprintf(stderr,"graphic_reset at courier medium 14 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	 
   }
   
   
   
   /* fprintf(stderr,"graphic_reset font height and width is %d %d %d \n", f_height, f_width, ifont_index); debug */	 
 
  
  /* This function has been removed from the wstxpt_() in esp_draw.c and used here to modify the fonts  
   * changed by the user in graphic feedback area */ 
   gtk_widget_modify_font(graphic, pfd);	/* << ?? >> */
   
   pango_font_description_free(pfd);

  /*  ifsc=butn_fnt; itfsc=disp_fnt; imfsc=menu_fnt;
   lttyc=10;	<< this is a place holder >> */

  /* pass revised information back to the fortran side */
  /*  updview_(&ifsc,&itfsc,&imfsc,&b_left,&b_right,&b_top,&b_bottom,&gw,&gh,&lttyc); */
   
  return;
}


/* ************* Timer(msec) pause_for_milliseconds ********* */
void Timer(msec)   /* from xvmisc.c */
 int  msec;
{
  long usec;
  struct timeval time;

  if (msec <= 0) return;
  usec = (long) msec * 1000;
    
  time.tv_sec = usec / 1000000L;
  time.tv_usec = usec % 1000000L;
/* find PC equivalent... select(0, XV_FDTYPE NULL, XV_FDTYPE NULL, XV_FDTYPE NULL, &time); */
  return;
}

/* ************* pausems_() pause_for_milliseconds ********* */
void pausems_(msec)   /* from xvmisc.c */
 long int  *msec;
{
  int msecond;
  long usec;
  struct timeval time;

  msecond = (int) *msec;
  if (msecond <= 0) return;
  usec = (long) msecond * 1000;
    
  time.tv_sec = usec / 1000000L;
  time.tv_usec = usec % 1000000L;
/* find PC equivalent... select(0, XV_FDTYPE NULL, XV_FDTYPE NULL, XV_FDTYPE NULL, &time); */
  return;
}

/* ************* pauses_() pause_for_seconds ********* */
void pauses_(is)
  long int *is;
{
  int i;
  i = (int) *is;
  sleep((unsigned int) i );
  return;
}



/* curmodule - pass in info on the current application from fortran */
void curmodule_(fcmodule,len_fcmodule)
  char *fcmodule;	/* f77 application name */
  int  len_fcmodule;	/* length of string passed */
{
  int  l_fcmodule;	/* local length */

/* use the same logic as in curproject_ */
  strncpy(cappl,"    ",4);
  f_to_c_l(fcmodule,&len_fcmodule,&l_fcmodule); strncpy(cappl,fcmodule,(unsigned int)l_fcmodule);	/* copy to static */
  cappl[l_fcmodule] = '\0';
/* debug   fprintf(stderr,"the current application is %s\n",cappl); */ 
}

/* functions for passing .esprc capture commands */
/* capexgf - rectangular graphic capture feedback command */
void capexgf_(cmd,len_cmd)
  char      *cmd;         /* f77 message    */
  int  len_cmd;           /* length of string from f77    */
{
  int l_m = 0;
  f_to_c_l(cmd,&len_cmd,&l_m); strncpy(capt_wf_exe,cmd,(unsigned int)l_m);	/* copy to static array */
  capt_wf_exe[l_m] = '\0';
  return;
}

/* capextf - text feedback dump file */
void capextf_(cmd,len_cmd)
  char      *cmd;         /* f77 message    */
  int  len_cmd;           /* length of string from f77    */
{
  int  l_m;
  f_to_c_l(cmd,&len_cmd,&l_m); strncpy(capt_tf_file,cmd,(unsigned int)l_m);	/* copy to static array */
  capt_tf_file[l_m] = '\0';
  return;
}

/* capexall - save whole display image */
void capexall_(cmd,len_cmd)
  char      *cmd;         /* f77 message    */
  int  len_cmd;           /* length of string from f77    */
{
  int  l_m;
  f_to_c_l(cmd,&len_cmd,&l_m); strncpy(capt_all_exe,cmd,(unsigned int)l_m);	/* copy to static array */
  capt_all_exe[l_m] = '\0';
  return;
}

/* ********* userfonts_ set fonts for common display tasks ******* */
/* Pass font preferences from fortran. ifs for buttons and graphs,
 * itfs for text feedback and dialog, imfs for command menus
 */
void userfonts_(ifs,itfs,imfs)
long int *ifs,*itfs,*imfs;
{
 butn_fnt = (int) *ifs;	/* remember the button and graph text font size */
 disp_fnt = (int) *itfs;	/* dialogue and text feedback  */
 menu_fnt = (int) *imfs;	/* prefered menu font */
/* debug   fprintf(stderr,"setting font size to %d %d %d\n",butn_fnt,disp_fnt,menu_fnt); */
 return;
}

/*********** xavail_ *************************************** *
 * Function indicating if X libraries are available (GTK/X11)
 */
int ixavail_()
{
  return 1;
}

/*********** igraphiclib_ *************************************** *
 * Function indicating if GTK libraries are used
 * use a one for X11 and in esru.c set two and in esru_fc.f set 3.
 */
int igraphiclib_()
{
  return 2;  /* use a two for GTK */
}

