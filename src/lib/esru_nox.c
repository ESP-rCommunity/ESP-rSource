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
        f_to_c_l() confirm Fortran string length
        ckaccess_() finds access parameters of a file
        getfilelist_() return list of files in a folder
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
/* examples of varible width fonts */
/* #define FONT1 "-*-lucida-medium-r-*-*-10-*-*-*-*-*-*-*" */
/* #define FONT2 "-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*" */
/* #define FONT1 "-*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*" */
/* #define FONT2 "-*-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*" */

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
static box	cfgz,cfgn,cfgc,cfgdfn;	/* boxes for problem type */
static box	mouse,mouse1,mouse2,mouse3;	/* box for mouse button help */
static char mseb1h[10],mseb2h[10],mseb3h[10]; /* mouse help strings */
static int aziplus_left,aziminus_left,azi_left,elevplus_left,elevminus_left,elev_left; /* left of azi&elev boxes */
static int b_setup, l_setup, b_cpw, l_cpw, b_tut, l_tut;	/* ll of setup, copyright, tutorial boxs */
static int wire_left,capture_left,captext_left;	/* left of wire frame and capture control box */
static long int ocfgz,ocfgn,ocfgc,ocfgdfn; /* persistant toggles for problem type boxes */
static long int iiocfgz,iiocfgn,iiocfgc,iiocfgdfn; /* persistant toggles for problem type images */
static int disp_opened = 0;     /* flag for existance of text display box. */
static int dialogue_lines = 0;  /* number of lines of text in dialogue box */
static int disp_lines = 0;      /* number of lines of text in text display box */
static int menu_offset = 24;    /* character offset to right side of feedback  */
static int fbb_b_lines = 2;    /* character offset to bottom of feedback      */
static int tut_avail = 0;       /* tutorial box existence */
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

/* ********* Displays the ESRU logo. */
void showlogo_(itime,lix,liy)
long int *itime,*lix, *liy; /* persistance, position from lower left of the 3dviewing image area */
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

/* ******  Place configuration buttons on screen ********** */
void opencfg_(cfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz,iicfgn,iicfgc,iicfgdfn)
  long int *cfg_type;	/* type of problem configuration */
  long int *icfgz,*icfgn,*icfgc,*icfgdfn;     /* toggles for zones/networks/control/domain boxes */
  long int *iicfgz,*iicfgn,*iicfgc,*iicfgdfn;	/* indicators for associated images */
{
 long int eyex,eyey,sym,sz;  /* centre for image symbols and symbol index and size */
 long int saved_font;
 int bh,hdl;
 int oocfgz = (int) *icfgz;	/* toggle for zones button */
 int iioocfgz = (int) *iicfgz;	/* toggle for zones images */
 int oocfgn = (int) *icfgn;	/* toggle for network button */
 int iioocfgn = (int) *iicfgn;	/* toggle for network images */
 int oocfgc = (int) *icfgc;	/* toggle for control button */
 int iioocfgc = (int) *iicfgc;	/* toggle for control images */
 int oocfgdfn = (int) *icfgdfn;	/* toggle for domain button */
 int iioocfgdfn = (int) *iicfgdfn;	/* toggle for domain images */
 ocfgz = oocfgz; ocfgn = oocfgn; ocfgc = oocfgc; ocfgdfn = oocfgdfn; /* remember toggles */
 iiocfgz = iioocfgz; iiocfgn = iioocfgn; iiocfgc = iioocfgc; iiocfgdfn = iioocfgdfn; /* remember images */

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

/* ************** f_to_c_l() confirm Fortran string length *************** */
/*
 Since the string length automaticly passed between Fortran and C
 tends to represent the "defined" string length rather than the actual
 string length here is a bit of code to start at the "defined" end
 and work backwards to find the last non-blank character position.
*/
void f_to_c_l(msg,f_len,len)
  char    *msg;         /* character string */
  int     *f_len;
  int	  *len;  /* fortran string length,
                    found position of last non blank character */
{
  int lm, sl, n;       /* local string lengths found by test  */
  int found;
  sl = (int) strlen(msg);
  if( sl == (int) *f_len ){
    n = (int) *f_len;
  } else if( sl > (int) *f_len ) {
    n = (int) *f_len;
  } else if( sl < (int) *f_len ) {
    n = sl;
  }
  found = 0;
  while(n > 0 && found==0 ) {
    n--;
    if ( msg[n] != ' ') found = 1;
  }
  if (found==1) {
    lm = n+1;
  } else if (found==0 && n == 0) {
    lm = 1;
  }
  *len = (int) lm;    /* for nox version use (int) rather than (gint) */

} /* f_to_c_l */

/* ****** ckaccess_() finds access parameters of a file ********* */
/* Given a file or folder name returns laccess = 1 if user cannot write
 * it and laccess = 0 if use can write. Also traps error state if file
 * or folder name in error/does not exist etc. Return folder=1 if the
 * name is a folder, otherwise 0.
 */
void ckaccess_(folder,laccess,lerr,fname,len)
char *fname;
int len;	/* string length from fortran */
long int *lerr,*laccess;
long int *folder;
{
 int ilen,i,ok;
 int iaccessu,iaccessg,iaccesso,ifolder;
 int ist_uid,ist_gid,u_uid,u_euid,g_uid,g_euid;
 char name2[80];
 struct stat st;

/* Work with copy of file name. */
  ilen = 0;
  f_to_c_l(fname,&len,&ilen); strncpy(name2,fname,(unsigned int)ilen); name2[ilen] = '\0';
/*   fprintf(stderr,"file is %s %d %d \n",name2,ilen,len); */
  i = stat(name2, &st);
  iaccessu = (st.st_mode&0200); /* returns 0 if u-w */
  iaccessg = (st.st_mode&0020); /* returns 0 if g-w */
  iaccesso = (st.st_mode&0002); /* returns 0 if o-w */
  ifolder = (st.st_mode&0x4000); /* returns nonzero if a folder */
  if (ifolder != 0) {
/* fprintf(stderr,"stat says folder %s\n",name2); */ 
    *folder = 1;  /* name is a folder */
  } else {
    *folder = 0;
  }
  ist_uid = st.st_uid;  ist_gid = st.st_gid; /* the file or folders owner */

#ifdef MINGW
  if (i != 0) {
/*  fprintf(stderr,"Windows - any one can write to it %d\n",iaccesso ); */
     *folder = 1;  /* name is probably a folder */
     ok=0; *laccess = ok; return; /* anyone can write anyway, set and return */
  }
#else	/* something other than windows */   
  u_uid = (long) getuid();  u_euid = (long) geteuid(); /* the user id and effective user id */
  g_uid = (long) getgid();  g_euid = (long) getegid(); /* the group id and effective group id */

/*   fprintf(stderr,"access u g o data %d %d %d\n",iaccessu,iaccessg,iaccesso); */
/*   fprintf(stderr,"id data %d %d %d %d %d %d \n",ist_uid,ist_gid,u_uid,u_euid,g_uid,g_euid); */
/*   fprintf(stderr,"access data %d %d \n",st.st_mode,i);  */
#endif    

/* logic test for permissions */
  ok = 1;   /* first assume that we can't write to it */
  if (i != 0) {
/* */ fprintf(stderr,"unable to get stats on %s\n",name2);
    *lerr = 1;
    *folder = -1;
    ok=1; *laccess = ok; /* error on stats better not write to it */
    return;
  } else {
    *lerr = 0;
  }
  if ((iaccessu == 0) && (iaccessg == 0) && (iaccesso == 0)) {
/* fprintf(stderr,"no one can write to it %d %d %d\n",iaccessu,iaccessg,iaccesso ); */
    ok=1; *laccess = ok; return; /* no one can write to it */
  }
  if (iaccesso != 0 ) {
/* fprintf(stderr,"any one can write to it %d\n",iaccesso ); */
    ok=0; *laccess = ok; return; /* anyone can write anyway */
  }
  if ((ist_uid == u_uid) && iaccessu != 0 ) {
/* fprintf(stderr,"user can write to it %d\n",iaccessu); */
    ok=0; *laccess = ok;  return; /* ok same user and owner and can write */
  }
  if ((ist_uid != u_uid) && (ist_gid ==g_uid) && iaccessg != 0 ) {
/* fprintf(stderr,"group can write to it %d\n",iaccessg); */
    ok=0;  *laccess = ok;  return; /* ok same group and can write */
  }
  *laccess = ok;  /* fell through logic so return */
/* fprintf(stderr,"fell through access logic %d\n",iaccessg); */
  return;
}

/* ********* Return list of files in a folder ********* */
void getfilelist_(folder,act,flist,nwflist,nflist,lenfolder,lenact,lenflist)
  char *folder;	/* folder name passed from fortran */
  char *act;  /* action to take as follows:
	`dir` get list of folders, `fil` get list of files in the folder,
	`cfg` get list of configuration files, `ctl` get list of control files,
	`afn` get list of air flow networks, `gnf` get list of graphic network files,
	`res` get list of results files, `mfr` get list of mass flow results 
	`geo` get list of geometry files, `obs` obstructions file,
	`opr` get list of operation files, `con` get list of construction files,
        `vwf` get list of viewfactor files, `tmc` zone optics file,
	`htc` get list of convection regime files,
	`shd` get list of shading files, `cgc` casual gain control files,
	`gdb` generic database, `gda` (gdba) ascii generic database
	`dba` ascii database,
	`dbb` binary database or climate file,
	`xbm` X bitmaps, `gif` gif image files.
        `ipv` IPV definition file, `rep` IPV report file
        `qac` QA contents file, `zip` cflo3 zip (geometry) file. 
    NOTE: the size of the 1st array index in char file_list must be edited to be
    the same as the parameter MFFOLD in include/espriou.h
 */
	
  char *flist;	/* f77 array of returned folders or file names */
  long int nwflist[];	/* array of character widths for each folder or file name. */
  long int *nflist;	/* number of folders or file names */
  int lenfolder,lenact,lenflist;	/* fortran passed lengths */
{
/* Local variables   */
  int ilen,ialen,i,ic,num,ipos;       /* local indicies  */
  struct stat st;		/* structure for file permissions */
  struct dirent *dirt;		/* structure for directory contents */
  DIR *dir;		/* directory structure (from dirent.h) */
  int ifolder;		/* if nonzero then stat structure says it is a folder */
  int foundone;		/* set to one if a file matches criteria. */

/* local working string arrays */
  char *locflist = flist;
  char *locact = act;
  int locnflist = *nflist;
  static char file_list[100][73];	/* character arrays to hold folder or file names. */
  char name2[80];	/* buffer for folder name */
  char act2[8];	/* buffer for act */

/* this function takes file or folder names generated by C calls
   and passes them back to fortran as an array of strings. It
   also passess back the actual width of each string so that
   the returned string can be copied to another fortran string
   array for proper use. As this is coded, fortran lnblnk calls
   to the returned string *flist will report 72 char width, no matter
   what the actual length of the string. Thus the use of nwflist[]
*/
/* clear the local return string array and reset nflist */
  locnflist = 0;
  for ( i = 0; i < 99; i++ ) {
    nwflist[i] = (long int) 0;
    strcpy(file_list[i],
      "                                                                         ");
  }
/* Work with copy of folder name. */
  f_to_c_l(folder,&lenfolder,&ilen); strncpy(name2,folder,(unsigned int)ilen); name2[ilen] = '\0';
  f_to_c_l(act,&lenact,&ialen); strncpy(act2,act,(unsigned int)ialen); act2[ialen] = '\0';
  i = stat(name2, &st);
  ifolder = (st.st_mode&0x4000); /* returns nonzero if a folder */
  if (ifolder == 0) {
    fprintf(stderr,"folder passed %s %d %d is not a folder.\n",name2,ilen,lenfolder); 
    return;
  }
  if (lenact == 0) {
    fprintf(stderr,"requested file or folder listing action was blank.\n"); 
    return;
  }

/* Open the folder */
  i = stat(name2, &st);
  ifolder = (st.st_mode&0x4000); /* returns nonzero if a folder */
  if ( ifolder != 0 ) {
    if (NULL == (dir = opendir(name2))) {
	fprintf(stderr, "%s: cannot read.\n", name2);
	return;
    }
  }
/* Carry on getting information until all folder items have been looked at */
    while (dirt = readdir(dir)) {
      foundone = 0;
/* don't bother with . and .. entries */
      if (strcmp(".", dirt->d_name) == 0 || strcmp("..", dirt->d_name) == 0) continue;

/* debug     printf("current %s\n", dirt->d_name); */
      if(strcmp("dir",act2)== 0) { /* If request for folders only, then build this list. */
        i = stat(dirt->d_name, &st);
        ifolder = (st.st_mode&0x4000); /* returns nonzero if a folder */
/* debug     fprintf(stderr,"%s returns ifolder %d\n", dirt->d_name,ifolder); */
        if (ifolder != 0 ) foundone = 1;
      } else if(strcmp("fil",act2)== 0) { /* If request for files only, then build this list. */
        i = stat(dirt->d_name, &st);
        ifolder = (st.st_mode&0x8000); /* returns zero if a regular file */
/* debug     fprintf(stderr,"%s returns ifolder %d\n", dirt->d_name,ifolder); */
        if (ifolder == 0 ) foundone = 1;
      } else if(strcmp("cfg",act2)== 0) { /* If request for cfg files only, then build this list. */
        if (strstr(dirt->d_name,".cfg")) foundone = 1;
      } else if(strcmp("ctl",act2)== 0) { /* If request for control files only, then build this list. */
        if (strstr(dirt->d_name,".ctl")) foundone = 1;
      } else if(strcmp("afn",act2)== 0) { /* If request for flow files only, then build this list. */
        if (strstr(dirt->d_name,".afn")) foundone = 1;
      } else if(strcmp("gnf",act2)== 0) { /* If request for network files only, then build this list. */
        if (strstr(dirt->d_name,".gnf")) foundone = 1;
      } else if(strcmp("res",act2)== 0) { /* If request for results files only, then build this list. */
        if (strstr(dirt->d_name,".res")) foundone = 1;
      } else if(strcmp("mfr",act2)== 0) { /* If request for flow results files only, then build this list. */
        if (strstr(dirt->d_name,".mfr")) foundone = 1;
      } else if(strcmp("geo",act2)== 0) { /* If request for geometry files only, then build this list. */
        if (strstr(dirt->d_name,".geo")) foundone = 1;
      } else if(strcmp("opr",act2)== 0) { /* If request for operation files only, then build this list. */
        if (strstr(dirt->d_name,".opr")) foundone = 1;
      } else if(strcmp("obs",act2)== 0) { /* If request for obstrucion files only, then build this list. */
        if (strstr(dirt->d_name,".obs")) foundone = 1;
      } else if(strcmp("vwf",act2)== 0) { /* If request for viewfactor files only, then build this list. */
        if (strstr(dirt->d_name,".vwf")) foundone = 1;
      } else if(strcmp("tmc",act2)== 0) { /* If request for zone optics files only, then build this list. */
        if (strstr(dirt->d_name,".tmc")) foundone = 1;
      } else if(strcmp("shd",act2)== 0) { /* If request for zone shading files only, then build this list. */
        if (strstr(dirt->d_name,".shd")) foundone = 1;
      } else if(strcmp("cgc",act2)== 0) { /* If request for cas gain ctl files only, then build this list. */
        if (strstr(dirt->d_name,".cgc")) foundone = 1;
      } else if(strcmp("htc",act2)== 0) { /* If request for convection regime files only, then build this list. */
        if (strstr(dirt->d_name,".htc")) foundone = 1;
        if (strstr(dirt->d_name,".hcc")) foundone = 1;
      } else if(strcmp("con",act2)== 0) { /* If request for construction files only, then build this list. */
        if (strstr(dirt->d_name,".con")) {
/* debug  fprintf(stderr,"%s is a zone constr file\n", dirt->d_name); */
          foundone = 1;
        }
      } else if(strcmp("gdb",act2)== 0) { /* If request for generic db only, then build this list. */
        if (strstr(dirt->d_name,".gdb")) foundone = 1;
      } else if(strcmp("gda",act2)== 0) { /* If request for asci generic db only, then build this list. */
        if (strstr(dirt->d_name,".gdba")) foundone = 1;
      } else if(strcmp("dba",act2)== 0) { /* If request for asci legacy db only, then build this list. */
        if (strstr(dirt->d_name,".dba")) foundone = 1;
        if (strstr(dirt->d_name,".a"))   foundone = 1;
      } else if(strcmp("xbm",act2)== 0) { /* If request for X pixmap only, then build this list. */
        if (strstr(dirt->d_name,".xbm")) foundone = 1;
        if (strstr(dirt->d_name,".XBM")) foundone = 1;
      } else if(strcmp("gif",act2)== 0) { /* If request for gif images only, then build this list. */
        if (strstr(dirt->d_name,".gif")) foundone = 1;
        if (strstr(dirt->d_name,".GIF")) foundone = 1;
      } else if(strcmp("ipv",act2)== 0) { /* If request for IPV definitions only, then build this list. */
        if (strstr(dirt->d_name,".ipv")) foundone = 1;
        if (strstr(dirt->d_name,".IPV")) foundone = 1;
      } else if(strcmp("rep",act2)== 0) { /* If request for IPV report only, then build this list. */
        if (strstr(dirt->d_name,".rep")) foundone = 1;
      } else if(strcmp("qac",act2)== 0) { /* If request for QA contents only, then build this list. */
        if (strstr(dirt->d_name,".contents")) foundone = 1;
      } else if(strcmp("zip",act2)== 0) { /* If request for Zip or cflo3, then build this list. */
        if (strstr(dirt->d_name,".zip")) foundone = 1;
/* debug   fprintf(stderr,"%s is a zip file\n", dirt->d_name); */
      }

/* the terminal index should match the size of the file_list[] buffer and that
 * should match the MFFOLD parameter on the fortran side! 
 */
      if ((foundone == 1) && (locnflist <= 99)) {	/* add d_name to the fixed string array */
        ic = (int) strlen(dirt->d_name);
        nwflist[locnflist] = ic;	/* remember width of d_name */
        strcpy(file_list[locnflist],dirt->d_name);
        locnflist = locnflist +1;
      }
    }
/* get recovered folder or file names back into the original fortran array */
    ipos = 0;
    strcpy(locflist,
  "                                                                         ");
  for(num = 0; num < locnflist; num++) {	/* for each recovered string...  */
    strncpy(&locflist[ipos],file_list[num],(unsigned int)lenflist);	/* copy to local array */
    ipos=ipos+lenflist;
  }
  *flist = *locflist;	/* copy locflist back to flist */
  *nflist = locnflist;
  return;
}


