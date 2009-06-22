/* Miscel non-graphic functions taken from esru_x.c
   
   winfnt(n)   changes the font (4 different sizes 0,1,2,3)
   tchild_() return child process terminal info.
   Timer(msec) pause_for_milliseconds
   pausems_() pause_for_milliseconds
   pauses_() pause_for_seconds
   wwcopen_() open ww commands output file 
   wwcclose_() Close and mark end or ww commands file
   wwcsetstart_() indicate start of a set of drawing commands
   wwcsetend_()	 indicate end of a set of drawing commands
   curproject_() pass in info on the current project from fortran
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
#include <esp-r.h>
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
   ifont_index = (gint) *font_index;
   PangoFontDescription *pfd;	/* to hold test font */
   
   long int ifsc,itfsc,imfsc,lttyc; /* parameters must be long int */

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

/* *************** tchild_() return child process terminal info. ******** */
void tchild_(cterm)
long int *cterm;           /* child terminal type  */
{
  *cterm = child_ter;
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

/* ******* f_to_c_l() confirm Fortran string length is in esru_nox.c *************** */


/*  wwcopen_() open ww commands output file */
void wwcopen_(name,len)
char *name;
int len;	/* string length from fortran */
{
 gint ilen;
 char name2[80];
 wwc_ok = 1;   /* set flag to echo drawing commands to wwc */

/* 
 * Terminate at fortran length, find actual string length
 * and then reterminate.
 */
  ilen = 0;
  f_to_c_l(name,&len,&ilen); strncpy(name2,name,(unsigned int)ilen); name2[ilen]='\0';
  if ((wwc = fopen(name2,"w"))==NULL) {
    fprintf(stderr,"could not open wwc file %s\n",name2);
    exit(1);
  }
  return;
}

/* wwcclose_() Close and mark endww commands file if one has been setup */
void wwcclose_(name,len)
char *name;
int len;	/* string length from fortran */
{
 gint ilen;
 char name2[80];
  ilen = 0;
  f_to_c_l(name,&len,&ilen); strncpy(name2,name,(unsigned int)ilen); name2[ilen] = '\0';
  if ( wwc_ok == 1) {
    wwc_ok = 0;   /* reset flag to not echo drawing commands to wwc */
    fprintf(wwc,"*end_wwc\t%s\n",name2);
    fclose(wwc);
  } else {
    fprintf(stderr,"ww commands file never opened...\n");
  }
  return;
}

void wwcsetstart_()	/* indicate start of a set of drawing commands */
{
 if ( wwc_ok == 1) fprintf(wwc,"*start_set\n");
  return;
}

void wwcsetend_()		/* indicate end of a set of drawing commands */
{
 if ( wwc_ok == 1) fprintf(wwc,"*end_set\n");
  return;
}


/* curproject_() - pass in info on the current project from fortran */
void curproject_(fcfgroot,fpath,fupath,fimgpth,fdocpth,ftmppth,ibrowse,
  len_root,len_fpath,len_fupath,len_fimgpth,len_fdocpth,len_ftmppth)
  char *fcfgroot;	/* f77 project root name    */
  char *fpath;	/* f77 project path    */
  char *fupath;	/* f77 users path    */
  char *fimgpth;	/* f77 relative path to images    */
  char *fdocpth;	/* f77 relative path to documents    */
  char *ftmppth;	/* f77 relative path to scratch folder    */
  long int *ibrowse;	/* if = 0 then user owns, if = 1 user browsing */
  int  len_root,len_fpath,len_fupath,len_fimgpth,len_fdocpth,len_ftmppth;	/* length of strings from f77  */
{
  int  l_root,l_fpath,l_fupath,l_fimgpth,l_fdocpth,l_ftmppth;

  l_root = l_fpath = l_fupath = l_fimgpth = l_fdocpth = l_ftmppth =0;
  browse = (gint) *ibrowse;
  strcpy(cfgroot,"                       ");
  f_to_c_l(fcfgroot,&len_root,&l_root); strncpy(cfgroot,fcfgroot,(unsigned int)l_root);	/* copy to static */
  cfgroot[l_root] = '\0';
  strcpy(imgpth, "                       ");
  f_to_c_l(fimgpth,&len_fimgpth,&l_fimgpth); strncpy(imgpth,fimgpth,(unsigned int)l_fimgpth);	/* copy to static */
  imgpth[l_fimgpth] = '\0';
  strcpy(docpth, "                       ");
  f_to_c_l(fdocpth,&len_fdocpth,&l_fdocpth); strncpy(docpth,fdocpth,(unsigned int)l_fdocpth);	/* copy to static */
  docpth[l_fdocpth] = '\0';
  strcpy(tmppth, "                       ");
  f_to_c_l(ftmppth,&len_ftmppth,&l_ftmppth); strncpy(tmppth,ftmppth,(unsigned int)l_ftmppth);	/* copy to static */
  tmppth[l_ftmppth] = '\0';
  strcpy(path,
      "                                                                         ");
  f_to_c_l(fpath,&len_fpath,&l_fpath); strncpy(path,fpath,(unsigned int)l_fpath);	/* copy to static */
  path[l_fpath] = '\0';
  strcpy(upath,
      "                                                                         ");
  f_to_c_l(fupath,&len_fupath,&l_fupath); strncpy(upath,fupath,(unsigned int)l_fupath);	/* copy to static */
  upath[l_fupath] = '\0';
/* debug  fprintf(stderr,"cfgroot %s\n",cfgroot);  */
/* debug  fprintf(stderr,"imgpth %s\n",imgpth);  */
/* debug  fprintf(stderr,"docpth %s\n",docpth);  */ 
/* debug  fprintf(stderr,"path %s\n",path);  */
/* debug  fprintf(stderr,"upath %s\n",upath);  */
/* debug  fprintf(stderr,"browse %d\n",browse);  */
  return;
}

/* curmodule - pass in info on the current application from fortran */
void curmodule_(fcmodule,len_fcmodule)
  char *fcmodule;	/* f77 application name */
  int  len_fcmodule;	/* length of string passed */
{
  int  l_fcmodule;	/* local length */

/* use the same logic as in curproject_ */
  strcpy(cappl,"    ");
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

