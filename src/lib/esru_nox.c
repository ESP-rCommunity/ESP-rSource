/*
esru_nox.c	(making no use of raw X calls) 
These routines are called from ESP-r fortran code for
intialisation and dummy routines for graphics. The routines are :-

	jwinint		:- initialise X window and check fonts
	winlod(name,ix,iy)
			:- loads an image to the screen
	windcl(n,ir,ig,ib)
			:- define the colour 'n' in RGB using ir,ig,ib
	feedbox		:- setup feedback display box
	viewtextwwc_(msg,line,side,size,len)
                        :- writes viewtext attributes to file.
	viewtext_(msg,line,side,size,len)
                        :- displays a line of text within the viewing
                           box with size and location parameters
	opengdisp_(menu_char,displ_l,dialogue_l,gdw,gdh)
                        :- opens a scrolling text display area.
	opensetup_()    :- place environment button on screen.
	aux_menu()
                        :-  test for mouse click in other portions of the screen.
	refreshenv_()
                        :-  pass back window information to fortran common.
*/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/time.h>
#include "wwxlegacy.h"

#define XV_FDTYPE (fd_set *)

/* global data types */

int cur_cursor = -1;

static int curstype;	/* xv cursor type */
static int mono;	/* true if displaying grayscale */
static int theScreen;
static int defaultVis = 0;         /* true if using DefaultVisual */
static int ipwait_flag = 0;        /* remember i/o mode */


static unsigned int dispDEEP;
/* indicies used for various colours */ 
static unsigned long fg, bg, bd, bw;

char *getenv ();

#define DEBUG 0               /* <<recompile this to 0 for silent>> */

static long int current_font;		/* standard font */
static long int disp_fnt;     /*   font for text display box */
static long int butn_fnt = 1; /*   button font size     */
static long int menu_fnt = 1;  /*  preferred command menu font */
static long int small_fnt = 0; /*  smallest button or box font size.    */
static int START_HEIGHT = 530;  /* nominal pixel height of window */
static int START_WIDTH = 695;   /* nominal pixel width of window */
static int START_ULX = STARTX;  /* nominal upper left pixel position of window */
static int START_ULY = STARTY;  /* nominal upper left pixel position of window */
static int pause_len;           /* pause loop length */
static int f_height,f_width;	/* of biggest char */
static int f_baseline,f_lbearing;	/* num pixels under baseline and from origin to left edge of char */
/* boxes outer viewing box, inner viewing box, dialogue box, text display, and scroll bar. */
static box	dbx1, viewbx, msgbx, askbx, disp, scroll, scrollbar;
static box      scrollv, scrollbarv,scrollh, scrollbarh;
static box	fbb, tut, setup, cpw; /* feedback box background, tutorial, setup, copyright */
static box	wire, capture, captext ;	/* wireframe ctl, capture graphics & capture text button */
static box	azi,aziplus,aziminus;	/* buttons for view azimuth changes */
static box	elev,elevplus,elevminus;	/* buttons for view elevation changes */
static box     altb,querb,defb,okb;	/* boxes for alts,query help, default, confirm */
static box     updown_text;	/* box for resizing text feedback */
static box     a,b,c,d,e,f,g;	/* boxes for multiple choices */
static box	cfgz,cfgs,cfgn,cfgc,cfgpln,cfgeln,cfgren;	/* boxes for problem type */
static box	cfgfab,cfgbeh,cfgsim;
static box	mouse,mouse1,mouse2,mouse3;	                      /* box for mouse button help */
static char mseb1h[10],mseb2h[10],mseb3h[10];                         /* mouse help strings */
static int aziplus_left,aziminus_left,azi_left,elevplus_left,elevminus_left,elev_left; /* left of azi&elev boxes */
static int b_setup, l_setup, b_cpw, l_cpw;	                       /* ll of setup, copyright boxs */
static int wire_left,capture_left,captext_left;	                       /* left of wire frame and capture control box */
static long int ocfgz,ocfgs,ocfgn,ocfgc,ocfgpln,ocfgeln;     /* persistant toggles model feature boxes */
static long int ocfgren,ocfgfab,ocfgbeh,ocfgsim; 
static int disp_opened = 0;     /* flag for existance of text display box. */
static int dialogue_lines = 0;  /* number of lines of text in dialogue box */
static int disp_lines = 0;      /* number of lines of text in text display box */
static int menu_offset = 24;    /* character offset to right side of feedback  */
static int fbb_b_lines = 2;    /* character offset to bottom of feedback      */
static int setup_avail = 0;     /* setup box existence */
static int cpw_avail = 0;       /* copyright box existence */
static int mouse_avail = 0;     /* mouse help box existence */
static long int cfg_boxs = -1;	/* current cfg type */
static int capture_avail = 0;   /* capture button box existence */
static int azi_avail = 0;	/* azimuth button box existence */
FILE *tf_dump;			/* file for text dump */
static int ask_len;             /* width of the input box */ 
static int last_visibility_event = 0; /* keep track of previous visibility change */
static int tfb_limit,tfb_limtty,tfb_line;   /* as in spad common block */

/* remember prompts so can redisplay if window uncovered or resized */

static char edit_list[PROFMA_LEN][96];	/* character arrays to be edited in fprofma_ */
static char display_list[PROFMA_LEN][125];	/* character arrays used within fprofma_ */
static int display_lines = 0;	/* current number of proforma display strings */
static char m_list[MENU_LIST_LEN][125];	/* character arrays for menu buffer */
static char mtype_list[MENU_LIST_LEN];	/* character array representing m_list array use */
static int m_width = 0;		/* current menu max line length */
static int menu_lines = 0;		/* current number of active menu lines */

extern char path[73];	/* f77 project path    */

/* flag for network graphics routines*/
static int network_gpc;

/* general menu to appear in network graphics mode  */
static char *grdgphcmenu[] = { "functions: zoom IN  ",
                        "           zoom OUT ",
                        "           grid +   ",
                        "           grid -   ",
                        "           grid ON  ",
                        "           grid OFF ",
                        "           snap ON  ",
                        "           snap OFF ",
                        "           close    ", 0};
/* menu to appear in network graphics mode for a selected object  */
static char *objgphcmenu[] = { "functions: rotate     ",
                        "           flip vert  ",
                        "           flip horiz  ",
                        "           delete     ",
                        "           close      ", 0};

char *t0;
static char font_0[60], font_1[60], font_2[60], font_3[60], font_4[60], font_5[60];


/* ********** Define colour *********** */
void windcl_(n,ir,ig,ib)
int *n, *ir, *ig, *ib;
{
/* this routine should define the color n to RBG value ir,ig,ib */
  int def;
  def = (*ir << 16) | (*ig << 8) | *ib ;
  /* do something with def equiv to ww copack(def,*n,COSET); */
  return;
}

/* ********* Loads an xbitmap file into the pixmap exbit and displays
   in the 3dviewing image area. itime is the number of miliseconds
   that bitmap should be displayed. To make persistent use itime =0
   lix is pixel offset from left of graphic display area and liy is
   ?? offset from ?? of display area. */
void winlod_(name,itime,lix,liy,len)
char *name;
long int *lix, *liy, *itime;
int len;
{ 
 return;
}

/* ********* Loads some or all of an xbitmap file into the pixmap
   exbit and displays it in the 3dviewing image area. lreqx is the
   start x pixel in the file bitmap, lreqy is the start y pixel in
   the file bitmap, lreqwidth is the width of pixels to take from the
   file, lreqheight is the height pixels to take from the file.
   lix is pixel offset from left of graphic display area and liy is
   vertical offset from the top of display area. */

/* If the bitmap is displayed then the following data is returned
   to the calling function: boxulx,boxuly,boxlrx,boxlry with are
   the pixel coordinates of the upper left and lower right corners. */

/* note that this call does not restore the image under the bitmap. */
void winlodpart_(name,lreqx,lreqy,lreqwidth,lreqheight,lix,liy,boxulx,boxuly,boxlrx,boxlry,len)
char *name;
long int *lreqx, *lreqy, *lreqwidth, *lreqheight, *lix, *liy;
long int *boxulx, *boxuly, *boxlrx, *boxlry;
int len;
{ 
 return;
}

/* ********* Checks loading some or all of an xbitmap file without
   displaying it. lreqx is the start x pixel in the file bitmap,
   lreqy is the start y pixel in the file bitmap, lreqwidth is the
   width of pixels to take from the file, lreqheight is the height
   pixels to take from the file.
   lix is pixel offset from left of graphic display area and liy is
   vertical offset from the top of display area. */

/* The following data is returned to the calling function: boxulx,boxuly,boxlrx,boxlry with are
   the pixel coordinates of the upper left and lower right corners. */

/* note that this call does not restore the image under the bitmap. */
void checklodpart_(name,lreqx,lreqy,lreqwidth,lreqheight,lix,liy,boxulx,boxuly,boxlrx,boxlry,len)
char *name;
long int *lreqx, *lreqy, *lreqwidth, *lreqheight, *lix, *liy;
long int *boxulx, *boxuly, *boxlrx, *boxlry;
int len;
{ 
 return;
}

/*
 * inside: return true if point x,y is inside the box.
 *  The top left edges are inside the box, the bottom right are not.
 */
int xboxinside(b,x,y) box b; int x,y;{
	return(x>=b.b_left && x<b.b_right
	    && y>=b.b_top && y<b.b_bottom);
}

/* ******* passes current state of spad common block (to help refresh of text feedback) */
void espad_(llimit,llimtty,lline)
  long int *llimit,*llimtty,*lline;   /* same as spad common block */
{
  tfb_limit = (int) *llimit; tfb_limtty = (int) *llimtty; tfb_line = (int) *lline;
  return;
}

/* ******  Routine to trak mouse in view box. ********** */
void trackview_(ichar,irx,iry)
 long int *ichar,*irx,*iry;	/* character returned, mouse position	*/
{
  return;
}

/* *************** ESRU circular arc routine. *************** */
/*
 This draws a circular arc based on the size and position in user units
 and transforms into pixel positions via static values passed into
 the C code via a previous call to linescale. Note: to shade the
 edge of an arc call twice with di=0. then di=0.5.
*/
void ecirarc_(x,y,ths,thf,r,num,di)
  float *x, *y,*ths,*thf,*r,*di;
  long int *num;	
{
  return;
}

/* *************** ESRU pop-up menu box. *************** */
/* 
 This function takes current menu text and displays it in a box
 and waits for the user to press a key or click a mouse to select
 a command choice. A long list paging is supported, by returning
 control to calling code to supply revised menu array.
 If the menu will not fit in the width of the box then a 
 smaller font will be used. 
*/
#define OFFEND		(-1)	/* no menu item selected */

void evwmenu_(titleptr,	/* title for menu                   */
  impx,impy,		/* menu position (upper right)      */
  iwth,			/* menu character width to print    */
  irpx,irpy,		/* selected x and y mouse position  */
  ino,			/* selected item number: if ino<0 dont show menu */	
  ipflg,uresp,		/* implement paging, miscel user response */	
  len_title		/* length of title (from f77 compiler) */
  )
char	*titleptr;
long int	*impx,*impy,*iwth,*irpx,*irpy;
long int	*ino,*ipflg,*uresp;
int		len_title;
{
/* Local variables   */
  return; 
}

/* *************** Extents of ESRU pop-up menu box. *************** */
/*
 This function takes current menu text and returns the width of the
 active menu area and the pixel width of the longest line of text.
*/

void extentsvwmenu_(titleptr,	/* title for menu                   */
  iwth,			/* menu character width to print    */
  ipixwthma,ipixwthll,  /* pixel with allocated for menu area and longest line  */
  ivfw,ivfwsp,ivfwul,   /* avg character widths */
  len_title		/* length of title (from f77 compiler) */
  )
char	*titleptr;
long int   *iwth,*ipixwthma,*ipixwthll;
long int   *ivfw,*ivfwsp,*ivfwul; /* avarage font widths */
int        len_title;
{
/* Local variables   */
  long int saved_font, use_font, changed_font, label_font;
  int   mib_width,i,iy,lt1,iw;
  int   lt,ltsp,ltul;  /* lengths of test strings */
  int   vfw,vfwsp,vfwul; /* avarage font widths */
  int fonth,imw,foundul;
  int pixwthll,pixwthbl,pixwthul;
  
  return;
}

/* ****************** auxulliary menu   
  Used to test for mouse click in other portions of the screen.
*/
/*
int aux_menu(event)  XEvent *event; {

  int but_rlse = 0;
  return (but_rlse);
}
*/

/* ********** refresh display *************** */
void refreshenv_()
{
   return;
} /* refreshenv */

/* ******  Place model feature buttons on screen ********** */
void opencfg_(cfg_type,icfgz,icfgs,icfgnet,icfgc,icfgpln,icfgeln,icfgren,icfgfab,icfgbeh,icfgsim)
  long int *cfg_type;	/* type of problem configuration */
  long int *icfgz,*icfgs,*icfgnet,*icfgc,*icfgpln,*icfgeln,*icfgren; /* toggles for feature boxes */
  long int *icfgfab,*icfgbeh,*icfgsim; 
{
 long int saved_font;
 int bh,hdl;
 int oocfgz = (int) *icfgz;	        /* toggle for zones button */
 int oocfgs = (int) *icfgs;	        /* toggle for Context button */
 int oocfgn = (int) *icfgnet;	        /* toggle for Fluid flow button */
 int oocfgc = (int) *icfgc;	        /* toggle for Control button */
 int oocfgpln = (int) *icfgpln;	        /* toggle for HAVC button */
 int oocfgeln = (int) *icfgeln;	        /* toggle for Electrical button */
 int oocfgren = (int) *icfgren;	        /* toggle for Renewables button */
 int oocfgfab = (int) *icfgfab;	        /* toggle for Enhanced fabric button */
 int oocfgbeh = (int) *icfgbeh;	        /* toggle for Behaviour button */
 int oocfgsim = (int) *icfgsim;	        /* toggle for automation button */
 ocfgz = oocfgz; ocfgs = oocfgs; ocfgn = oocfgn; ocfgc = oocfgc; /* remember toggles */
 ocfgpln = oocfgpln; ocfgeln = oocfgeln; ocfgren = oocfgren; ocfgfab = oocfgfab; 
 ocfgbeh = oocfgbeh; ocfgsim = oocfgsim;
  return;
} /* opencfg */


/* notify pixel position of right edge of graphic display
   note that the fbb box structure contains int data types
   and these need to be explicitly cast to long int */
void findrtb_(right,top,bottom)
  long int *right, *top, *bottom;
{
  *right = (long int) fbb.b_right;
  *top = (long int) fbb.b_top;
  *bottom = (long int) fbb.b_bottom;
  return;
}


/* instruct which interaction mode to use */
void ipset(flag) int flag;{
 ipwait_flag = flag;
}

/* return structure of the current event. */
/* int inpwait(event)  XEvent *event; {

  if(ipwait_flag==0) {
    fprintf(stderr,"inpwait 0: no ipset on?\n");
    exit(1);
  }

  return (inpwait);
}
*/

/* ****** Switch network graphics ON or off ****** */
void nwkgflg_(ngf)
long int *ngf;
{
network_gpc= (int) *ngf;
}

/* *************** ESRU fortan callable proforma for checkboxs and editing. *************** */
/* 
 This function takes fortran strings to display and edit, arrays indicating which items
 are labels, which are associated with tick boxes and which are to be edited. It then
 builds the proforma and waits for the user to select one or more tick boxes and editing one or
 more items. No paging supported. 
*/
void fprofma_(sstr,nstr,title,list,nlist,listtypes,impx,impy,impcwth,swidth,listact,ino,nhelp,lensstr,lentitle,lenlist)
char	*sstr;	/* f77 array of strings to edit */
long int *nstr;	/* number of strings to edit */
char	*title;	/* title   */
char	*list;	/* f77 array of displayed text */
long int *nlist;	/* number of strings to display */
char	*listtypes;	/* characters representing how each item is treated
			t = tickbox, e = edit string, l = label (no action),
			- = horizontal separator (no action), c = call-back
			which returns listact[] of 2 for processing by calling code. */
long int *impx,*impy;	/* popup position (lower left)      */
long int *impcwth;	/* width (chars) of the proforma */
long int swidth[];	/* array of user specified max editing display width (chars) for sstr */
long int listact[];	/* array of ints (matching list) which are set=1
                   if tick boxes are checked and returned to calling
                   function for decoding */
long int *ino;	/* item selected if >-1 and <=ilen, ino= -1 nothing, ino=ilen+1 ok button */
long int *nhelp; /* number of help_lines */
int lensstr,lentitle,lenlist;	/* fortrant passed lengths */
{
  return;
}



