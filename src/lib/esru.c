/* Miscel non-graphic functions taken from esru_x.c
   
   winfnt(n)   changes the font (4 different sizes fixed and proportional 0,1,2,3,4,5,6,7)
   Timer(msec) pause_for_milliseconds
   pausems_() pause_for_milliseconds
   pauses_() pause_for_seconds
   curmodule_() pass in info on the current application from fortran
   userfonts() set fonts for common display tasks
   defaultfonts() set fonts for common display tasks
*/
   
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/time.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include "esp-r.h"
#include <commons.h>

#define XV_FDTYPE (fd_set *)

/* circa Dec. 2005, MinGW apparently no longer offers 'sleep'.
   Alias to MS windows Sleep, which takes milliseconds 
   instead of seconds
#ifdef MINGW
#include <windows.h>
#define sleep(s) Sleep(s * 1000)
#endif  */

extern FILE *wwc;
extern gint f_height;
extern gint f_width;
extern long int d_disp_fnt = 1; /* default font for text display box */
extern long int d_butn_fnt = 4; /* default button font size */
extern long int d_menu_fnt = 5; /* default command menu font */

/* flag for network graphics routines*/
static int network_gpc;

/* flags for network graphics - start in select mode */
static int nselect = 1;	/* if 1 then allow select or deselect of icons */
static int nconnect = 0;	/* if 1 then user in connection add mode */
static int ndata = 0;	/* if 1 allow data editing */
static long int isnap=1;
static long int igrid=1;

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

/* Follow similar patter to X11 implementataion - 0/1/2/3 fixed width and 4/5/6/7 proportional.
 */

  if (ifont_index == 0 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 8");
   f_height = font_calculations_array[courier_small].f_height;
   f_width  = font_calculations_array[courier_small].f_width;
//   fprintf(stderr,"graphic_reset at Mono medium 8 change font height and width is %d %d %d\n", f_height, f_width, ifont_index);	//debug
    
  } else if (ifont_index == 1 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 9");
   f_height = font_calculations_array[courier_medium].f_height;
   f_width  = font_calculations_array[courier_medium].f_width;
//   fprintf(stderr,"graphic_reset at Mono medium 9 change font height and width is %d %d %d\n", f_height, f_width,ifont_index );	//debug
      
   } else if (ifont_index == 2 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 10");
   f_height = font_calculations_array[courier_large].f_height;
   f_width  = font_calculations_array[courier_large].f_width;
//   fprintf(stderr,"graphic_reset at Mono medium 10 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	

   } else if (ifont_index == 3 ) {
   pfd = pango_font_description_from_string("Monospace,Medium 11");
   f_height = font_calculations_array[courier_largest].f_height;
   f_width  = font_calculations_array[courier_largest].f_width;
//   fprintf(stderr,"graphic_reset at Mono medium 11 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	
   }
   
   else if (ifont_index == 4) {
   pfd = pango_font_description_from_string("Serif,Medium 8");
   f_height = font_calculations_array[serif_small].f_height;
   f_width  = font_calculations_array[serif_small].f_width;
   //fprintf(stderr,"graphic_reset at serif medium 8 change font height and width is %d %d %d \n ", f_height, f_width, ifont_index);	//debug	 
      
   } 
   else if (ifont_index == 5  ) {
   pfd = pango_font_description_from_string("Serif,Medium 9");
   f_height = font_calculations_array[serif_medium].f_height;
   f_width  = font_calculations_array[serif_medium].f_width;
   // fprintf(stderr,"graphic_reset at serif medium 9 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	 
   }
   else if (ifont_index == 6 ) {
   pfd = pango_font_description_from_string("Serif,Medium 10");
   f_height = font_calculations_array[serif_large].f_height;
   f_width  = font_calculations_array[serif_large].f_width;
     //fprintf(stderr,"graphic_reset at serif medium 10 change font height and width is %d %d %d\n", f_height, f_width, ifont_index);	//debug	  
   }
   else if (ifont_index == 7 ) {
   pfd = pango_font_description_from_string("Serif,Medium 11");
   f_height = font_calculations_array[serif_largest].f_height;
   f_width  = font_calculations_array[serif_largest].f_width;
   //fprintf(stderr,"graphic_reset at serif medium 11 change font height and width is %d %d %d \n", f_height, f_width, ifont_index);	//debug	 
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
#ifdef MINGW
/*  Sleep((unsigned int) msec ); */
#else
  select(0, XV_FDTYPE NULL, XV_FDTYPE NULL, XV_FDTYPE NULL, &time);
#endif
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
#ifdef MINGW
/*  Sleep((unsigned int) msecond ); */
#else
  select(0, XV_FDTYPE NULL, XV_FDTYPE NULL, XV_FDTYPE NULL, &time);
#endif
  return;
}

/* ************* pauses_() pause_for_seconds ********* */
void pauses_(is)
  long int *is;
{
  int i;
  i = (int) *is;
#ifdef MINGW
/*  Sleep((unsigned int) msecond *1000); */
#else
  sleep((unsigned int) i );
#endif
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

/********* Switch between network drawing modes *********/
void nwksmod_(sel,con,dat)
long int *sel;
long int *con;
long int *dat;
{
nselect=*sel;	/* if 1 then user can select/unselect icons */
nconnect=*con;
ndata=*dat;
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

/* *************** set default fonts for current application ******** */
/* Pass default font preferences from fortran. ifs for buttons and graphs,
 * itfs for text feedback and dialog, imfs for command menus
 */
void defaultfonts_(ifsd,itfsd,imfsd)
long int *ifsd,*itfsd,*imfsd;
{
 d_butn_fnt = (int) *ifsd;	/* remember the button and graph text font size */
 d_disp_fnt = (int) *itfsd;	/* dialogue and text feedback  */
 d_menu_fnt = (int) *imfsd;	/* prefered menu font */
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

