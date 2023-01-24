/*
esru_x.c	(making use of raw X calls)
These routines are called from ESP-r fortran code for
intialisation and graphics, using ww. The routines are :-

	sizeint         :- passess window size & position
	sizehwxy	:- passess window size, w/h ratio & position
	setpause_(n)	:- set length of pause
	jwinint		:- initialise X window and check fonts
	winclr		:- clears screen
	winfin		:- closes ww window
	winlod(name,ix,iy)
			:- loads an image to the screen
	windcl(n,ir,ig,ib)
			:- define the colour 'n' in RGB using ir,ig,ib
	winscl(n)	:- set current colour to n
        winenqcl(act,n,xcolid) :- enquire about forground colour
        userfonts(ifs,itfs,imfs) :- set fonts
        defaultfonts(ifsd,itfsd,imfsd):- set application default fonts
	feedbox		:- setup feedback display box
	winfnt(n)	:- changes the font (various sizes 0...7)
	wstxpt(x,y,buff,len):-outputs a string beginning at pixel x and y.
	textatxywwc(x,y,buff,act,n,len):-outputs to file a coloured string beginning at pixel x&y.
	textatxy(x,y,buff,act,n,len):-outputs a coloured string beginning at pixel x&y.
	textsizeatxy(x,y,buff,size,act,n,len):-outputs a sized coloured string beginning at pixel x&y.
	win3dwwc(menu_char,cl,cr,ct,cb,vl,vr,vt,vb,gw,gh)
                        :- opens a viewing box taking into account menu
                           width and dialogue box.
	win3d(menu_char,cl,cr,ct,cb,vl,vr,vt,vb,gw,gh)
                        :- opens a viewing box taking into account menu
                           width and dialogue box.
	win3dclr_()     :- clear viewing box.
	viewtextwwc_(msg,line,side,size,len)
                        :- writes viewtext attributes to file.
	viewtext_(msg,line,side,size,len)
                        :- displays a line of text within the viewing
                           box with size and location parameters
	openaskbox_(msg1,msg2,asklen,len1,len2)
                        :- creates a text input box within the dialogue
                           area positioned to match the msg's passed.
	askdialog_(sstr,idef,iquery,asklen)
                        :- controls input of strings in the dialogue area.
	msgbox_(msg1,msg2,len1,len2)
                        :- places prompts in the dialogue box with the
                           same syntax used in USRMSG.
	continuebox_(msg1,msg2,opta,len1,len2,len3)
                        :- places prompts in dialogue box along with
                           an action choice.
	abcdefbox_(msg1,msg2,opta,optb,optc,optd,opte,optf,optg,ok,len1,len2,len3,len4,len5,len6,len7,len8,len9)
                        :- places prompts in dialogue box along with
                           two-seven user supplied choices.
	opengdisp_(menu_char,displ_l,dialogue_l,gdw,gdh)
                        :- opens a scrolling text display area.
	egdisp_(msg,iw,line,len)
                        :- writes text in the scrolling text display area.
	egdispclr_()    :- clears scrolling text display at EPAGEW.
	elinewwc_(x,y,operation)
                        :- writes eline attributes to file.
	eline_(x,y,operation)
                        :- draws or moves a line @ pixel location x y.
	edline_(x1,y1,x2,y2,ipdis)
                        :- draws a dotted line from x1 y1 to x2 y2.
	edash_(x1,y1,x2,y2,ipdis)
                        :- draws a dashed line from x1 y1 to x2 y2.
	echainwwc_(x1,y1,x2,y2,ipdis)
                        :- writes echain attributes to file.
	echain_(x1,y1,x2,y2,ipdis)
                        :- draws a chained line from x1 y1 to x2 y2.
	edwlinewwc_(x1,y1,x2,y2)
                        :- writes edwline attributes to file.
	edwline_(x1,y1,x2,y2)
                        :- draws a double width line from x1 y1 to x2 y2.
	eswlinewwc_(x1,y1,x2,y2)
                        :- writes eswline attributes to file.
	eswline_(x1,y1,x2,y2)
                        :- draws a single width line from x1 y1 to x2 y2.
	ecirc_(x,y,rad,operation)
                        :- draws a filled or open circle @ location x y.
	esymbolwwc_(x,y,sym,size)
                        :- writes esymbol attributes to file.
	esymbol_(x,y,sym,size)
                        :- draws one of 32 symbols at location x y.
	updhelp_(items,nitmsptr,iw,len_items)
                        :- updates help text for subsequent display.
	egphelp_(impx,impy)
                        :- opens and displays current help text in a box.
	axiscale_(gw,gh,xmn,xmx,ymn,ymx,xsc,ysc,sca,xadd,yadd)
                        :- determines scaling parameters for horizontal
                           and vertical axis.
	linescale-(loff,ladd,lscale,boff,badd,bscale)
                        :- stores scaling parameters for lines.
	vrtaxis_(ymn,ymx,offl,offb,offt,yadd,sca,mode,side,msg,mlen)
                        :- draws a vertical axis (tic & labels on right
                           or left side).
	horaxis_(xmn,xmx,offl,offr,offb,xadd,sca,mode,msg,mlen)
                        :- draws a horizontal axis with tic marks and
                           labels.
	labelstr(n,val,WticC,sstr)
                        :- generates an appropriate label for the value
                           passed.  INTERNAL.
	opensetup_()    :- place font button on screen.
	opencpw_()      :- place copyright button on screen.
	aux_menu()
                        :-  test for mouse click in other portions of the screen.
	refreshenv_()
                        :-  pass back window information to fortran common.

  Examples of Xft fonts that can be used:
  strncpy(font_0,"Ubuntu Mono-8:medium",20);
  strncpy(font_1,"Ubuntu Mono-9:medium",20);
  strncpy(font_2,"Ubuntu Mono-10:medium",21);
  strncpy(font_3,"Ubuntu Mono-11:medium",21);
  strncpy(font_4,"Ubuntu-8:medium",15);
  strncpy(font_5,"Ubuntu-9:medium",15);
  strncpy(font_6,"Ubuntu-10:medium",16);
  strncpy(font_7,"Ubuntu-11:medium",16);
  strncpy(font_0,"Liberation Mono-8:medium",24);
  strncpy(font_1,"Liberation Mono-9:medium",24);
  strncpy(font_2,"Liberation Mono-10:medium",25);
  strncpy(font_3,"Liberation Mono-11:medium",25);
  strncpy(font_4,"Liberation Sans-8:style=Regular",31);
  strncpy(font_5,"Liberation Sans-9:style=Regular",31);
  strncpy(font_6,"Liberation Sans-10:style=Regular",32);
  strncpy(font_7,"Liberation Sans-11:style=Regular",32);
  strncpy(font_0,"DejaVu Sans Mono-8:medium",25);
  strncpy(font_1,"DejaVu Sans Mono-9:medium",25);
  strncpy(font_2,"DejaVu Sans Mono-10:medium",26);
  strncpy(font_3,"DejaVu Sans Mono-11:medium",26);
  strncpy(font_4,"DejaVu Sans-8:style=Book",24);
  strncpy(font_5,"DejaVu Sans-9:style=Book",24);
  strncpy(font_6,"DejaVu Sans-10:style=Book",25);
  strncpy(font_7,"DejaVu Sans-11:style=Book",25);
  
*/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "wwcut.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/time.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
/* #include "editpop.xbm" */

/* shift this inside only the required functions 
#ifdef MINGW
#include <windows.h>
#define sleep(s) Sleep(s * 1000)
#endif  */

#define XV_FDTYPE (fd_set *)

#define gray25_width 8
#define gray25_height 8
static unsigned char gray25_bits[] = {
   0x88, 0x22, 0x88, 0x22, 0x88, 0x22, 0x88, 0x22};

/* external definitions in the Fortran code */
extern gnwkquery_();
extern wirepk_();
extern profgrdump_();
extern cpwpk_();
extern chgazi_();     /* in esrucom/common3dv.F */
extern chgelev_();    /* in esrucom/common3dv.F */
extern chgpan_();     /* in esrucom/common3dv.F */
extern optview_();    /* in esrucom/common3dv.F */
extern chgzoom_();    /* in esrucom/common3dv.F */
extern chgeye_();     /* in esrucom/common3dv.F */
extern chgsun_();     /* in esrucom/common3dv.F */
extern chgzonpik_();  /* in esrucom/common3dv.F */
extern redraw_();    /* in esrucom/common3dv.F */
extern chgzonpikarray_();
extern proftxdump_();
extern nwkslctc_();
extern gconad_();
extern gridupdt_();
extern nwkupdtpos_();
extern icntfm_();
extern cfgpk_();
extern updview_();
extern aux_menu();

/* global data types */
Display  *theDisp;
Colormap theCmap;
Visual *theVisual;
Window  win;
GC  theGC;
XGCValues  gcv;
XRenderColor render_color;
XftColor xft_zscale[100]; /* to match the array zscale */
XftColor xft_gscale[85];  /* to match the array gscale */
XftColor xft_cscale[50];  /* to match the array cscale */
XftColor xft_gmenuhl,xft_gmodbg,xft_gpopfr,xft_gfeedfr,xft_ginvert,xft_grey50,xft_grey43;
XftColor xft_bg,xft_white,xft_black;
XftFont  *fst,*fst_0,*fst_1,*fst_2,*fst_3,*fst_4,*fst_5,*fst_6,*fst_7;
XftDraw *ftdraw;
XftColor xft_color;
XSizeHints  xsh;
XSetWindowAttributes xswa;
XWindowAttributes xwa;
Window rootW, parent_win;
static Cursor arrow_cursor, cross_cursor, zoom_cursor, wait_cursor, inviso;

int cur_cursor = -1;

static int curstype;	/* xv cursor type */
static int mono;	/* true if displaying grayscale */
static int theScreen;
static int defaultVis = 0;         /* true if using DefaultVisual */
static int ipwait_flag = 0;        /* remember i/o mode */


static long int mdepth = 0;  /* to pass to fortran: sceen depth */
static unsigned int dispDEEP;
/* indicies used for various colours */
static unsigned long fg, bg, bd, bw, white, black, infofg, infobg;
static unsigned long gmenuhl, gpopfr, gfeedfr, ginvert, gmodbg, grey50, grey43;
static unsigned long cscale[49], zscale[100], gscale[85];

char *getenv ();

#define DEBUG 0               /* <<recompile this to 0 for silent>> */

static long int current_font;   /* standard font */
static long int disp_fnt;       /* font for text display box */
static long int box_fnt = 4;    /* font for embedded boxes     */
static long int butn_fnt = 4;   /* button font size     */
static long int menu_fnt = 5;   /* preferred command menu font */
static long int small_fnt = 0;  /* smallest button or box font size.    */
extern long int d_disp_fnt = 1; /* default font for text display box */
extern long int d_butn_fnt = 4; /* default button font size */
extern long int d_menu_fnt = 5; /* default command menu font */
static int START_HEIGHT = 530;  /* nominal pixel height of window */
static int START_WIDTH = 695;   /* nominal pixel width of window */
static int START_ULX = STARTX;  /* nominal upper left pixel position of window */
static int START_ULY = STARTY;  /* nominal upper left pixel position of window */
static int xold, yold;		/* current 'pen' position */
static int pause_len;           /* pause loop length */
static int f_height,f_width;	/* of biggest char */
static int f_baseline,f_lbearing;	/* num pixels under baseline and from origin to left edge of char */
/* boxes outer viewing box, inner viewing box, dialogue box, text display, and scroll bar. */
static box	dbx1, viewbx, msgbx, askbx, disp, scroll, scrollbar;
static box      scrollv, scrollbarv,scrollh, scrollbarh;
static box	fbb, setup, cpw; /* feedback box background, setup, copyright */
static box	wire, capture, captext ;	/* wireframe ctl, capture graphics & capture text button */
static box	azi,aziplus,aziminus;	/* buttons for view azimuth changes */
static box	elev,elevplus,elevminus;	/* buttons for view elevation changes */
static box     altb,altc,querb,defb,okb;	/* boxes for alts,query help, default, confirm */
static box     updown_text;	                                        /* box for resizing text feedback */
static box     a,b,c,d,e,f,g;	                                        /* boxes for multiple choices */
static box      cfgs,cfgnet,cfgc,cfgpln,cfgeln,cfgren;	/* boxes for model features */
static box	cfgfab,cfgbeh,cfgsim;
static box	mouse,mouse1,mouse2,mouse3;	                        /* box for mouse button help */
static char mseb1h[10],mseb2h[10],mseb3h[10];                           /* mouse help strings */
static int aziplus_left,aziminus_left,azi_left,elevplus_left,elevminus_left,elev_left; /* left of azi&elev boxes */
static int b_setup, l_setup, b_cpw, l_cpw;	                        /* ll of setup, copyright boxs */
static int wire_left,capture_left,captext_left;	                        /* left of wire frame and capture control box */
static long int ocfgz,ocfgs,ocfgnet,ocfgc,ocfgpln;          /* persistant toggles for problem type boxes */
static long int ocfgeln,ocfgren,ocfgfab,ocfgbeh,ocfgsim;
static int dbx1_avail = 0;      /* flag for existance of graphic display box */
static int c3dcl,c3dcr,c3dct,c3dcb; /* dbx1 char offsets left, right, top, bottom */
static int disp_opened = 0;     /* flag for existance of text display box. */
static int dialogue_lines = 0;  /* number of lines of text in dialogue box */
static int disp_lines = 0;      /* number of lines of text in text display box */
static int menu_offset = 24;    /* character offset to right side of feedback  */
static int fbb_b_lines = 2;    /* character offset to bottom of feedback      */
static int setup_avail = 0;     /* setup box existence */
static int cpw_avail = 0;       /* copyright box existence */
static int wire_avail = 0;      /* wireframe adjustment box existence */
static int mouse_avail = 0;     /* mouse help box existence */
static long int cfg_boxs = -1;	/* current cfg type */
static int capture_avail = 0;   /* capture button box existence */
static int azi_avail = 0;	/* azimuth button box existence */
static char capt_wf_exe[73];	/* command to execute for capture wire frame */
static char capt_tf_file[73];	/* file for text feedback buffer dump */
static char capt_all_exe[73];	/* command for capture all of display */
FILE *tf_dump;			/* file for text dump */
static int ask_len;             /* width of the input box */
static int x_off,y_off;         /* display X & Y offsets in pixels (see linescale) */
static float x_add,x_scale,y_add,y_scale;  /* X & Y axis scales and data offsets (see linescale) */
static int dash_on = 0;         /* toggle for dashed section of a line */
static int dash_rem = 0;        /* remaining pixel length in incomplete dash */
static int last_visibility_event = 0; /* keep track of previous visibility change */
static int tfb_limit,tfb_limtty,tfb_line;   /* as in spad common block */

/* remember prompts so can redisplay if window uncovered or resized */
static char *askmsg1,*askmsg2,*askmsg3; /* remembered prompt character strings */
static int asklm1, asklm2, asklm3, asklm4;    /* remember prompt lengths */
static int asklprompt; /* left position of lower prompt text */
static int asktprompt; /* left position of upper prompt text */

static char help_list[HELP_LIST_LEN][73];	/* character arrays to hold help to be displayed
				   by functions which include help */
static char edisp_list[EDISP_LIST_LEN][145];	/* character arrays for edisp buffer */
static char edit_list[PROFMA_LEN][96];	/* character arrays to be edited in fprofma_ */
static char display_list[PROFMA_LEN][125];	/* character arrays used within fprofma_ */
static int edisp_index = 0;	/* current position in edisp past list */
static int scroll_index = 0;	/* current position in scroll for edisp past list */
static int help_width = 0;	/* current help max line length */
static int help_lines = 0;	/* current number of active help lines */
static int edit_lines = 0;	/* current number of proforma editing strings */
static int display_lines = 0;	/* current number of proforma display strings */
static char m_list[MENU_LIST_LEN][125];	/* character arrays for menu buffer */
static char mtype_list[MENU_LIST_LEN];	/* character array representing m_list array use */
static int m_width = 0;		/* current menu max line length */
static int m_lines = 0;		/* current number of active menu lines */

static char cappl[5];	/* f77 application name */
/* static char cfgroot[33];	f77 project root name    */
/* static char path[73];	f77 project path    */
/* static char upath[73];	f77 users path    */
/* static char imgpth[25];	f77 relative path to images    */
/* static char docpth[25];	f77 relative path to documents    */
static int browse;	/* if = 0 then user owns, if = 1 user browsing */

/* flag for network graphics routines*/
static int network_gpc;

/* flags for network graphics - start in select mode */
static int nselect = 1;	/* if 1 then allow select or deselect of icons */
static int nconnect = 0;	/* if 1 then user in connection add mode */
static int ndata = 0;	/* if 1 allow data editing */
static long int isnap=1;
static long int igrid=1;

static int ter = -1;            /* terminal type passed on initial call (set initial
                            value to -1 which will be overwritten if run in
                            graphic mode - ie. text mode fail safe for any
                            system execution calls.  */
static int child_ter = -1;      /* child process terminal type  */

static char *fontmenu[] = {
                    "menu       : smaller    ",
                    "               : larger ",
                    "text feedback: smaller  ",
                    "               : larger ",
                    "dialogs   : smaller     ",
                    "              : larger  ",
                    "reset to defaults       ",
                    "dismiss                 ", 0 };

/* general menu to appear in network graphics mode second mouse button */
static char *netgm2menucd[] = { "functions: zoom IN  ",
                        "           zoom OUT ",
                        "           grid +   ",
                        "           grid -   ",
                        "           grid ON  ",
                        "           snap ON  ",
                        "           close    ", 0};
static char *netgm2menucc[] = { "functions: zoom IN  ",
                        "           zoom OUT ",
                        "           grid +   ",
                        "           grid -   ",
                        "           grid OFF ",
                        "           snap ON  ",
                        "           close    ", 0};
static char *netgm2menucb[] = { "functions: zoom IN  ",
                        "           zoom OUT ",
                        "           grid +   ",
                        "           grid -   ",
                        "           grid ON  ",
                        "           snap OFF ",
                        "           close    ", 0};
static char *netgm2menuca[] = { "functions: zoom IN  ",
                        "           zoom OUT ",
                        "           grid +   ",
                        "           grid -   ",
                        "           grid OFF ",
                        "           snap OFF ",
                        "           close    ", 0};

/* menu to appear in network graphics mode for a selected object second mouse button */
static char *objgphcmenu[] = { "functions: rotate     ",
                        "           flip vert  ",
                        "           flip horiz  ",
                        "           close      ", 0};

extern int  wwc_ok;   /* assume this set in esru_util.c */
extern int  wwc_macro;   /* assume this set in esru_util.c */
extern FILE *wwc;

char *t0;
static char font_0[80],font_1[80],font_2[80],font_3[80],font_4[80],font_5[80],font_6[80],font_7[80];

/* info about root Xwindow */
static int xrt_width, xrt_height;  /* same as xsh.width and xsh.height */
static char *bgstr,  *whitestr, *blackstr; /* init default colors */
static char* zscalestr[100] = { /* 100 colour names from rgb.txt to represent zone colours */
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
static char* cscalestr[49] = { /* colour scale RGB HEX values (to represent 49 steps of temperature) */
  "#FF0000","#FF1500","#FF2B00","#FF4000","#FF5500","#FF6A00","#FF8000","#FF9500","#FFAA00","#FFBF00",
  "#FFD500","#FFEA00","#FFFF00","#EAFF00","#D5FF00","#BFFF00","#AAFF00","#95FF00","#80FF00","#6AFF00",
  "#55FF00","#40FF00","#2AFF00","#15FF00","#00FF00","#00FF15","#00FF2B","#00FF40","#00FF55","#00FF6A",
  "#00FF80","#00FF95","#00FFAA","#00FFBF","#00FFD4","#00FFEA","#00FFFF","#00EAFF","#00D4FF","#00BFFF",
  "#00AAFF","#00AAFF","#00AAFF","#006AFF","#0055FF","#0040FF","#002BFF","#0015FF","#0000FF" };
/* strings to set interface greys 5 standards and 5 alternatives if allocation fails. */
static char* gintstr[13] = {
  "grey96","grey94","grey92","grey86","grey64","grey50",
  "grey95","grey93","grey91","grey85","grey63","grey49","grey43" };
/* 86 strings to set greay scale for zones if zscalestr allocation fails. */
static char* gscalestr[86] = {
  "grey97","grey96","grey95","grey94","grey93","grey92","grey91","grey90",
  "grey89","grey88","grey87","grey86","grey85","grey84","grey83","grey82","grey81","grey80",
  "grey79","grey78","grey77","grey76","grey75","grey74","grey73","grey72","grey71","grey70",
  "grey69","grey68","grey67","grey66","grey65","grey64","grey63","grey62","grey61","grey60",
  "grey59","grey58","grey57","grey56","grey55","grey54","grey53","grey52","grey51","grey50",
  "grey49","grey48","grey47","grey46","grey45","grey44","grey43","grey42","grey41","grey40",
  "grey39","grey38","grey37","grey36","grey35","grey34","grey33","grey32","grey31","grey30",
  "grey29","grey28","grey27","grey26","grey25","grey24","grey23","grey22","grey21","grey20",
  "grey19","grey18","grey17","grey16","grey15","grey14","grey13","grey12" };
static long int ncscale = 0; /* number of assigned colours in colour scale */
static long int ngscale = 0; /* number of assigned colours in grey scale */
static long int ngr = 0; /* number of assigned interface colours */
static long int izc = 0; /* number of assigned zone colours */


/* ************** Confirm string length *************** */
/*
 Find the actual string length via start at the "defined" end
 and work backwards to find the last non-blank character position.
*/
int clnblnk(msg)
  char    *msg;  /* character string */
{
  int lm, sl, n, found;       /* local string lengths found by test  */
  sl = (int) strlen(msg);
  n = sl;
  found = 0;
  lm = 0;
  while(n > 0 && found==0 ) {
    n--;
    if ( msg[n] != ' ') found = 1;
  }
  if (found==1) {
    lm = n+1;
  } else if (found==0 && n == 0) {
    lm = 1;
  }
  return lm;

} /* clnblnk */


/* *************** Initialise display size and position. *************** */
void sizeint_(size,ulx,uly)
long int *size;           /* pixel size for base window  */
long int *ulx,*uly;       /* pixel upper left corner of base window  */
{
  if ( *size >= 100 ) {
    START_HEIGHT = (int) *size;
    START_WIDTH = (int) *size*TEKX/TEKY;
  }
  if ( *ulx >= 1 )   START_ULX = (int) *ulx;
  if ( *uly >= 1 )   START_ULY = (int) *uly;
  return;
}

/* *************** Initialise display size, ratio and position. ***** */
void sizehwxy_(sizeh,sizew,ulx,uly)
long int *sizeh,*sizew;   /* pixel height & width for base window  */
long int *ulx,*uly;       /* pixel upper left corner of base window  */
{
  if ( *sizeh >= 100 ) START_HEIGHT = (int) *sizeh;
  if ( *sizew >= 100 ) START_WIDTH = (int) *sizew;
  if ( *ulx >= 1 )   START_ULX = (int) *ulx;
  if ( *uly >= 1 )   START_ULY = (int) *uly;
  return;
}


/* *************** Initialise display. *************** */
void jwinint_(term,msg,msglen)
long int *term;           /* terminal type from fortran  */
int msglen;         /* length of message passed from fortran */
char *msg;                /* window heading      */
{			  /* Initialize display  */
/*  unsigned long fg, bg, bd, bw, white, black; */
  int best, numvis;
  long flags;
  XEvent  event;
  XVisualInfo *vinfo, rvinfo;
  XColor ecdef;
  int argc;
  char **argv;
  XWMHints  xwmh;
  int i;               /* local string length  */
  bgstr = whitestr = blackstr = NULL;
  mono = 0;
  curstype = XC_top_left_arrow;

  theDisp = XOpenDisplay (0);
/* Open the display using the $DISPLAY env variable */
//if((theDisp = XOpenDisplay(NULL))==NULL) {
//  fprintf(stderr,"Can not open %s\n",XDisplayName(NULL));
//  exit(1);
//}

/* get structure of the current visual */
theScreen = DefaultScreen(theDisp);

// stuff like theVisual, defaultVis, dispDEEP might need to happen
// after call to XCreateSimpleWindow
theVisual = XDefaultVisual(theDisp, theScreen);
defaultVis = (XVisualIDFromVisual(theVisual) ==
       XVisualIDFromVisual(DefaultVisual(theDisp,DefaultScreen(theDisp))));
// fprintf(stderr,"defaultVis is %d \n",defaultVis);

dispDEEP  = DisplayPlanes(theDisp,theScreen);
// fprintf(stderr,"dispDEEP is %d \n",dispDEEP);
mdepth = (long int) dispDEEP;

theCmap   = DefaultColormap(theDisp, theScreen);
rootW     = RootWindow(theDisp,theScreen);

/* colours for Xft rendering */
render_color.red = 0; render_color.green =0; render_color.blue = 0; render_color.alpha = 0xffff;;

XftColorAllocValue (theDisp,
                    DefaultVisual(theDisp, theScreen),
                    DefaultColormap(theDisp, theScreen),
                    &render_color,&xft_color);

/* Load initial fonts, if environment variable not set then
 * default to standard fonts.
 */
t0=(char *) getenv("EFONT_0");
/* fprintf(stderr,"t0 is %s \n",t0); */
if ((t0 == NULL) || (t0  == "") || (strncmp(t0,"    ",4) == 0)) {
  strncpy(font_0,"Ubuntu Mono,Monospace-8:medium",30);
#ifdef OSX
  strncpy(font_0,"DejaVu Sans Mono-7:medium",25);
#endif
} else {
  strcpy(font_0,getenv("EFONT_0"));
}
if((fst_0 =  XftFontOpenName(theDisp,0,font_0)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s\n",DisplayString(theDisp),font_0);
  exit(1);
}
if (((t0=(char *) getenv("EFONT_1"))== NULL) || ((t0=(char *) getenv("EFONT_1"))== "")) {
  strncpy(font_1,"Ubuntu Mono,Monospace-9:medium",30);
#ifdef OSX
  strncpy(font_1,"DejaVu Sans Mono-8:medium",25);
#endif
} else {
  strcpy(font_1,getenv("EFONT_1"));
}
if((fst_1 = XftFontOpenName(theDisp,0,font_1)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s\n",DisplayString(theDisp),font_1);
  exit(1);
}
if (((t0=(char *) getenv("EFONT_2"))== NULL) || ((t0=(char *) getenv("EFONT_2"))== "")) {
  strncpy(font_2,"Ubuntu Mono,Monospace-10:medium",31);
#ifdef OSX
  strncpy(font_1,"DejaVu Sans Mono-9:medium",25);
#endif
} else {
  strcpy(font_2,getenv("EFONT_2"));
}
if((fst_2 = XftFontOpenName(theDisp,0,font_2)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s\n",DisplayString(theDisp),font_2);
  exit(1);
}
if (((t0=(char *) getenv("EFONT_3"))== NULL) || ((t0=(char *) getenv("EFONT_3"))== "")) {
  strncpy(font_3,"Ubuntu Mono,Monospace-11:medium",31);
#ifdef OSX
  strncpy(font_3,"DejaVu Sans Mono-10:medium",26);
#endif
} else {
  strcpy(font_3,getenv("EFONT_3"));
}
if((fst_3 = XftFontOpenName(theDisp,0,font_3)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s\n",DisplayString(theDisp),font_3);
  exit(1);
}

if (((t0=(char *) getenv("EFONT_4"))== NULL) || ((t0=(char *) getenv("EFONT_4"))== "")) {
  strncpy(font_4, "Ubuntu,Liberation Sans-8:medium",31);
} else {
  strcpy(font_4,getenv("EFONT_4"));
}
if((fst_4 = XftFontOpenName(theDisp,0,font_4)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s ...\n",DisplayString(theDisp),font_4);
  strncpy(font_4,"DejaVu Sans-8:style=Book",24);
  if((fst_4 =  XftFontOpenName(theDisp,0,font_4)) == NULL) {
    fprintf(stderr,"2nd choice font %s has not been found so quitting.\n",font_4);
    exit(1);
  }
}
if (((t0=(char *) getenv("EFONT_5"))== NULL) || ((t0=(char *) getenv("EFONT_5"))== "")) {
  strncpy(font_5,  "Ubuntu,Liberation Sans-9:medium",31);
} else {
  strcpy(font_5,getenv("EFONT_5"));
}
if((fst_5 = XftFontOpenName(theDisp,0,font_5)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s ...\n",DisplayString(theDisp),font_5);
  strncpy(font_5,"DejaVu Sans-9:style=Book",24);
  if((fst_5 = XftFontOpenName(theDisp,0,font_5)) == NULL) {
    fprintf(stderr,"2nd choice font %s has not been found so quitting.\n",font_5);
    exit(1);
  }
}
if (((t0=(char *) getenv("EFONT_6"))== NULL) || ((t0=(char *) getenv("EFONT_6"))== "")) {
  strncpy(font_6,  "Ubuntu,Liberation Sans-10:medium",32);
} else {
  strcpy(font_6,getenv("EFONT_6"));
}
if((fst_6 = XftFontOpenName(theDisp,0,font_6)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s ...\n",DisplayString(theDisp),font_6);
  strncpy(font_6,"DejaVu Sans-10:style=Book",25);
  if((fst_6 = XftFontOpenName(theDisp,0,font_6)) == NULL) {
    fprintf(stderr,"2nd choice font %s has not been found so quitting.\n",font_6);
    exit(1);
  }
}
if (((t0=(char *) getenv("EFONT_7"))== NULL) || ((t0=(char *) getenv("EFONT_7"))== "")) {
  strncpy(font_7,  "Ubuntu,Liberation Sans-11:medium",32);
} else {
  strcpy(font_7,getenv("EFONT_"));
}
if((fst_7 = XftFontOpenName(theDisp,0,font_7)) == NULL) {
  fprintf(stderr,"display %s doesn't know font %s ...\n",DisplayString(theDisp),font_7);
  strncpy(font_7,"DejaVu Sans-11:style=Book",25);
  if((fst_7 = XftFontOpenName(theDisp,0,font_7)) == NULL) {
    fprintf(stderr,"2nd choice font %s has not been found so quitting.\n",font_7);
    exit(1);
  }
}

/* Debug */
/*
fprintf(stderr,"startup: fonts are %s %s %s %s %s %s %s %s\n",font_0,font_1,font_2,font_3,font_4,font_5,font_6,font_7);
*/

ftdraw = XftDrawCreate(theDisp,win,theVisual,theCmap);  /* for Xft fonts */

/* set colours for the border, background and forground */
bd = WhitePixel(theDisp,theScreen);

/* init default colors (as used in Xv (RGB values as hex)) */
bgstr="#B2C0DC"; /* 198|213|226 */


/* from xv initial setup of colour data */
best = -1;
rvinfo.class  = TrueColor;
rvinfo.screen = theScreen;
flags = VisualClassMask | VisualScreenMask;

vinfo = XGetVisualInfo(theDisp, flags, &rvinfo, &numvis);
if (vinfo) {     /* look for a TrueColor, 24-bit or more (pref 24) */
  for (i=0, best = -1; i<numvis; i++) {
    if (vinfo[i].depth == 24) best = i;
    else if (vinfo[i].depth>24 && best<0) best = i;
  }
}

if (best == -1) {   /* look for a DirectColor, pref 24-bit */
  rvinfo.class = DirectColor;
  if (vinfo) XFree((char *) vinfo);
  vinfo = XGetVisualInfo(theDisp, flags, &rvinfo, &numvis);
  if (vinfo) {
    for (i=0, best = -1; i<numvis; i++) {
      if (vinfo[i].depth == 24) best = i;
      else if (vinfo[i].depth>24 && best<0) best = i;
    }
  }
}

if (vinfo) XFree((char *) vinfo);

/* create cursors as in xv */
  arrow_cursor = XCreateFontCursor(theDisp,(unsigned int) curstype);
  cross_cursor = XCreateFontCursor(theDisp,XC_crosshair);
  zoom_cursor  = XCreateFontCursor(theDisp,XC_pencil); /* XC_center_ptr */
  wait_cursor = XCreateFontCursor(theDisp,XC_watch);
  cur_cursor = -1;

  { /* create inviso cursor */
    Pixmap      pix;
    static char bits[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    XColor      cfg;

    cfg.red = cfg.green = cfg.blue = 0;
    pix = XCreateBitmapFromData(theDisp, rootW, bits, 8, 8);
    inviso = XCreatePixmapCursor(theDisp, pix, pix, &cfg, &cfg, 0,0);
    XFreePixmap(theDisp, pix);
  }

  /* set up white,black colors */
  if (defaultVis) {
    white = WhitePixel(theDisp,theScreen);
    render_color.red = 65535; render_color.green =65535; render_color.blue = 65535; render_color.alpha = 0xffff;
    XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_white);
    black = BlackPixel(theDisp,theScreen);
    render_color.red = 0; render_color.green =0; render_color.blue = 0; render_color.alpha = 0xffff;
    XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_black);
  }
  else {  /* also ensure there is an equivalent XftColor */
    ecdef.flags = DoRed | DoGreen | DoBlue;
    ecdef.red = ecdef.green = ecdef.blue = 0xffff;
    if (XAllocColor(theDisp, theCmap, &ecdef)) {
      white = ecdef.pixel;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_white);
    } else {
      white = 0xffffffff;    /* probably evil... */
    }
    ecdef.red = ecdef.green = ecdef.blue = 0x0000;
    if (XAllocColor(theDisp, theCmap, &ecdef)) {
      black = ecdef.pixel;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_black);
    } else {
      black = 0x00000000;    /* probably evil... */
    }
  }

  /* set up fg,bg colors */
  fg = black;   bg = white;
  if (bgstr && XParseColor(theDisp, theCmap, bgstr, &ecdef) && XAllocColor(theDisp, theCmap, &ecdef)) {
      bg = ecdef.pixel;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_bg);
  }

/* set interface grey colours using *gintstr[] (rgg.txt names = reflectance) */
  if (dispDEEP > 1) {   /* only if a reasonable display */
    if (XParseColor(theDisp,theCmap,gintstr[0],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      gmenuhl = ecdef.pixel; ngr=ngr+1;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_gmenuhl);
    } else {
      fprintf(stderr,"Problem colour %s\n",gintstr[0]);
      if (XParseColor(theDisp,theCmap,gintstr[6],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        gmenuhl = ecdef.pixel; ngr=ngr+1; } else { gmenuhl = white; ngr=ngr+1; }
    }
    if (XParseColor(theDisp,theCmap,gintstr[1],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      gmodbg = ecdef.pixel; ngr=ngr+1;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_gmodbg);
    } else {
      fprintf(stderr,"Problem colour %s\n",gintstr[1]);
      if (XParseColor(theDisp,theCmap,gintstr[7],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        gmodbg = ecdef.pixel; ngr=ngr+1; } else { gmodbg = gmenuhl; ngr=ngr+1; }
    }
    if (XParseColor(theDisp,theCmap,gintstr[2],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      gpopfr = ecdef.pixel; ngr=ngr+1;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_gpopfr);
    } else {
      fprintf(stderr,"Problem colour %s\n",gintstr[2]);
      if (XParseColor(theDisp,theCmap,gintstr[8],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        gpopfr = ecdef.pixel; ngr=ngr+1; } else { gpopfr = gmodbg; ngr=ngr+1; }
    }
    if (XParseColor(theDisp,theCmap,gintstr[3],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      gfeedfr = ecdef.pixel; ngr=ngr+1;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_gfeedfr);
    } else {
      fprintf(stderr,"Problem colour %s\n",gintstr[3]);
      if (XParseColor(theDisp,theCmap,gintstr[9],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        gfeedfr = ecdef.pixel; ngr=ngr+1; } else { gfeedfr = gpopfr; ngr=ngr+1; }
    }
    if (XParseColor(theDisp,theCmap,gintstr[4],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      ginvert = ecdef.pixel; ngr=ngr+1;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_ginvert);
    } else {
      fprintf(stderr,"Problem colour %s\n",gintstr[4]);
      if (XParseColor(theDisp,theCmap,gintstr[10],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        ginvert = ecdef.pixel; ngr=ngr+1; } else {  ginvert = black; ngr=ngr+1; }
    }
    if (XParseColor(theDisp,theCmap,gintstr[5],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      grey50 = ecdef.pixel; ngr=ngr+1;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_grey50);
    } else {
      fprintf(stderr,"Problem colour %s\n",gintstr[5]);
      if (XParseColor(theDisp,theCmap,gintstr[11],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        grey50 = ecdef.pixel; ngr=ngr+1; } else {  grey50 = black; ngr=ngr+1; }
    }
    if (XParseColor(theDisp,theCmap,gintstr[12],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      grey43 = ecdef.pixel; ngr=ngr+1;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_grey43);
    } else {
      fprintf(stderr,"Problem colour %s\n",gintstr[12]);
      if (XParseColor(theDisp,theCmap,gintstr[11],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        grey43 = ecdef.pixel; ngr=ngr+1; } else {  grey43 = black; ngr=ngr+1; }
    }
    ngr=ngr+2;	/* include black (ngr-1) and white (ngr) */
  }
/*  fprintf(stderr,"greys ngr %ld\n",ngr); */
  infofg = fg;  infobg = bg;

  /* if '-mono' not forced, determine if we're on a grey or color monitor */
  if (!mono) {
    if (theVisual->class == StaticGray || theVisual->class == GrayScale)
      mono = 1;
  }
  if(mono == 1) fprintf(stderr,"using monochrome mode %d \n",mono);

/* set border width and gap between text and edge */
bw = 2;

/* position and size */
xsh.flags = (PPosition | PSize);
xsh.height = START_HEIGHT;
xsh.width = START_WIDTH;
xrt_width = xsh.width; xrt_height = xsh.height;  /* remember */
xsh.x = START_ULX;
xsh.y = START_ULY;

/* create window */
  win = XCreateSimpleWindow(theDisp,DefaultRootWindow(theDisp),xsh.x,xsh.y,(unsigned int)xsh.width,(unsigned int)xsh.height,(unsigned int)bw,bd,gmodbg);

/* standard properties for the window manager */
  argv = NULL; argc = 0;
  XSetStandardProperties(theDisp,win,msg,msg,None,0,argc,&xsh);

/* structure for X window */
  xwmh.input = True;
  xwmh.initial_state = NormalState;
  xwmh.flags = (InputHint | StateHint);

  XSetWMHints(theDisp, win, &xwmh);

/* point window colourmap to the default colourmap */
  xswa.colormap = DefaultColormap(theDisp,theScreen);
  xswa.bit_gravity = NorthWestGravity;
  XChangeWindowAttributes(theDisp,win, (CWColormap | CWBitGravity), &xswa);

/* create the GC */
  gcv.foreground = fg;
  gcv.background = gmodbg;
  theGC = XCreateGC(theDisp,win, (GCForeground | GCBackground), &gcv);

/* event types */
  XSelectInput(theDisp,win, ExposureMask | ButtonPressMask | ButtonReleaseMask | KeyPressMask | PointerMotionMask | StructureNotifyMask | VisibilityChangeMask);

/* map window to make it visible */
  XMapWindow(theDisp,win);
  XWindowEvent (theDisp, win, ExposureMask | ButtonPressMask | KeyPressMask, &event);

  ipset(IPON); /* allow keyboard and mouse input */

  winclr_(); /* clear the root window */

  pause_len = 1000;  /* inital pause loop length */
  ter = (int) *term;        /* remember initial terminal type */
  child_ter = (int) *term;  /* set initial child process initial terminal type */

/* initial clear of help display list */
  for ( i = 0; i < HELP_LIST_LEN-1; i++ ) {
    strncpy(help_list[i],
      "                                                                        ",72);
  }
/* initial clear of edisp lines list */
  for ( i = 0; i < EDISP_LIST_LEN-1; i++ ) {
    strncpy(edisp_list[i],
    "                                                                                   ",82);
  }
/* initial clear of menu lines list and item type string */
  for ( i = 0; i < MENU_LIST_LEN-1; i++ ) {
    strncpy(m_list[i],
    "                                                                                   ",82);
  }
  strncpy(mtype_list,"                                        ",40);

/* initial clear of proforma editing and display lists */
  for ( i = 0; i < PROFMA_LEN-1; i++ ) {
    strncpy(edit_list[i],
      "                                                                        ",72);
    strncpy(display_list[i],
    "                                                                                   ",82);
  }

/* clear event stack of initial visibility and configure events */
  if(XPending(theDisp) > 0) {
/* debug   fprintf(stderr,"winint: events remaining %d\n",XPending(theDisp));  */
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor); /* turn on arrow cursor */
  return;
 }

/* ********* colour scale (50 or 25 steps) ******* */
void setcscale_() {
  int ic,ih;
  XColor ecdef;
/* assign colour scale to cscale (hex) array. */
  for (ic=0; ic<49; ic++) {
    if (XParseColor(theDisp,theCmap,cscalestr[ic],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        cscale[ic] = ecdef.pixel; ncscale=ncscale+1;
        render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
        XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_cscale[ic]);
    } else {
/*     fprintf(stderr,"Unable to create colour %s\n",cscalestr[ic]); */
     cscale[ic] = 0;
    }
  }
/* Some colours not allocated attempt half of the colours. Begin by freeing initial allocated set. */
  if ( ncscale <= 45 ) {
    for (ic=0; ic<ncscale; ic++) {
      if( cscale[ic] >= 1 ) XFreeColors(theDisp,theCmap,&cscale[ic],1,0L);
    }
    fprintf(stderr,"Trying reduced colour set\n");
    ncscale = 0;
    ih = -1;
    for (ic=0; ic<24; ic++) {
      ih = ih + 2;
      if (XParseColor(theDisp,theCmap,cscalestr[ih],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
         cscale[ic] = ecdef.pixel; ncscale=ncscale+1;
         render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
         XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_cscale[ic]);
      } else {
/* debug fprintf(stderr,"Unable to create colour %s\n",cscalestr[ic]);  */
         cscale[ic] = 0;
      }
    }
  }
  return;
}

/* ********* clear colour scale (50 or 25 steps) ******* */
void clrcscale_() {
  int ic;
  for (ic=0; ic<ncscale; ic++) {
    if( cscale[ic] >= 1 ) XFreeColors(theDisp,theCmap,&cscale[ic],1,0L);
  }
  ncscale = 0;
  return;
}

/* ********* grey scale (84 or 42 steps) ******* */
void setgscale_() {
  int ic,ih;
  XColor ecdef;
/* assign grey scale to gscale (hex) array. */
  for (ic=0; ic<84; ic++) {
    if (XParseColor(theDisp,theCmap,gscalestr[ic],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
        gscale[ic] = ecdef.pixel;
        render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
        XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_gscale[ic]);
        ngscale=ngscale+1;
    } else {
/* debug fprintf(stderr,"Unable to create colour %s\n",gscalestr[ic]); */
     gscale[ic] = 0;
    }
  }
/* Some colours not allocated attempt half of the colours. Begin by freeing initial allocated set. */
/* debug fprintf(stderr,"Created greys %ld\n",ngscale); */
  if ( ngscale <= 83 ) {
    for (ic=0; ic<ngscale; ic++) {
      if( gscale[ic] >= 1 ) XFreeColors(theDisp,theCmap,&gscale[ic],1,0L);
    }
    fprintf(stderr,"Trying reduced grey set\n");
    ngscale = 0;
    ih = -1;
    for (ic=0; ic<40; ic++) {
      ih = ih + 2;
      if (XParseColor(theDisp,theCmap,gscalestr[ih],&ecdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
         gscale[ic] = ecdef.pixel;
         render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
         XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_gscale[ic]);
         ngscale=ngscale+1;
      } else {
/* debug fprintf(stderr,"Unable to create colour %s\n",gscalestr[ic]); */
         gscale[ic] = 0;
      }
    }
  }
  return;
}

/* ********* clear grey scale (27 or 12 steps) ******* */
void clrgscale_() {
  int ic;
  for (ic=0; ic<ngscale; ic++) {
    if( gscale[ic] >= 1 ) XFreeColors(theDisp,theCmap,&gscale[ic],1,0L);
  }
  ngscale = 0;
  return;
}

/* ********* zone colour scale (81 steps) ******* */
void setzscale_() {
  int ic;
  XColor ecdef, sdef;
/* assign colours (zscale names) for zone graphing. */
  for (ic=0; ic<99; ic++) {
    XParseColor(theDisp,theCmap,zscalestr[ic],&ecdef);
//    fprintf(stderr,"zscale XParseColor %d %d %d %d\n",ic,ecdef.red,ecdef.green,ecdef.blue);
    if (XLookupColor(theDisp,theCmap,zscalestr[ic],&ecdef,&sdef) && XAllocColor(theDisp,theCmap,&ecdef)) {
      zscale[ic] = ecdef.pixel;
      render_color.red = ecdef.red; render_color.green =ecdef.green; render_color.blue = ecdef.blue; render_color.alpha = 0xffff;
      XftColorAllocValue (theDisp,theVisual,theCmap,&render_color,&xft_zscale[ic]);
      izc = izc + 1;
//      fprintf(stderr,"zscale %d %d %d %d\n",ic,ecdef.red,ecdef.green,ecdef.blue);
    } else {
      fprintf(stderr,"Unable to create colour %s\n",zscalestr[ic]);
      if ( ngscale >= ic && gscale[ic] >= 1 ) {
        zscale[ic] = gscale[ic];
      } else {
        zscale[ic] = black;
      }
      izc = izc + 1;
    }
  }
  // debug fprintf(stderr,"z colours %ld\n",izc);
  return;
}

/* notify fortran of monitor depth and nb interface greys, steps in colour scale, greyscale
   and zone scale */
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

/* ********** Set forground colour ************* */
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
 ic = (int) *n;
/* sets the current forground colour n depending on which active colour set being used */
  if(*act == 'g') {
     if (ic >= 0 && ic <= ngscale ) {
       XSetForeground(theDisp,theGC,gscale[ic]);
     } else {
       XSetForeground(theDisp,theGC,fg);
     }
  } else if(*act == 'z') {
     if (ic >= 0 && ic <= izc ) {
       XSetForeground(theDisp,theGC,zscale[ic]);
     } else {
       XSetForeground(theDisp,theGC,fg);
     }
  } else if(*act == 'c') {
     if (ic >= 0 && ic <= ncscale ) {
       XSetForeground(theDisp,theGC,cscale[ic]);
     } else {
       XSetForeground(theDisp,theGC,fg);
     }
  } else if(*act == 'i') {
     if (ic >= 0 && ic <= ngr ) {	/* including black and white */
       if (ic == 0) XSetForeground(theDisp,theGC,gmenuhl);
       if (ic == 1) XSetForeground(theDisp,theGC,gmodbg);
       if (ic == 2) XSetForeground(theDisp,theGC,gpopfr);
       if (ic == 3) XSetForeground(theDisp,theGC,gfeedfr);
       if (ic == 4) XSetForeground(theDisp,theGC,ginvert);
       if (ic == 5) XSetForeground(theDisp,theGC,grey50);
       if (ic == 6) XSetForeground(theDisp,theGC,grey43);
       if (ic == 7) XSetForeground(theDisp,theGC,black);
       if (ic == 8) XSetForeground(theDisp,theGC,white);
     } else {
       XSetForeground(theDisp,theGC,fg);
     }
  } else if(*act == '-') {
     XSetForeground(theDisp,theGC,fg);
     XSetBackground(theDisp,theGC,bg);
  }
  return;
}

/* ********** Enquire about forground colour ************* */
/* action "g" grey scale index 0 >= n <= ngscale */
/* action "z" zone index 0 >= n <= izc */
/* action "c" colour scale index  0 >= n <= ncscale */
/* action "i" interface greys index  0 >= n <= ngr */
/* action "-" resets the forground to standard fg colour */
void winenqcl_(act,n,xcolid)
char *act;  /* single character passed */
long int *n;	/* index as in winscl_ */
long int *xcolid;	/* index of colour used by X */
{
 int ic;
 ic = (int) *n;
/* sets the current forground colour n depending on which active colour set being used */
  if(*act == 'g') {
     if (ic >= 0 && ic <= ngscale ) {
       *xcolid = (long int)gscale[ic];
     } else {
       *xcolid = (long int)fg;
     }
  } else if(*act == 'z') {
     if (ic >= 0 && ic <= izc ) {
       *xcolid = (long int)zscale[ic];
     } else {
       *xcolid = (long int)fg;
     }
  } else if(*act == 'c') {
     if (ic >= 0 && ic <= ncscale ) {
       *xcolid = (long int)cscale[ic];
     } else {
       *xcolid = (long int)fg;
     }
  } else if(*act == 'i') {
     if (ic >= 0 && ic <= ngr ) {	/* including black and white */
       if (ic == 0) *xcolid = (long int)gmenuhl;
       if (ic == 1) *xcolid = (long int)gmodbg;
       if (ic == 2) *xcolid = (long int)gpopfr;
       if (ic == 3) *xcolid = (long int)gfeedfr;
       if (ic == 4) *xcolid = (long int)ginvert;
       if (ic == 5) *xcolid = (long int)grey50;
       if (ic == 6) *xcolid = (long int)black;
       if (ic == 7) *xcolid = (long int)white;
     } else {
       *xcolid = (long int)fg;
     }
  } else if(*act == '-') {
     *xcolid = (long int)fg;
  }
  return;
}

/* *************** set fonts for common display tasks ************ */
/* Pass font preferences from fortran. ifs for buttons and graphs,
 * itfs for text feedback and dialog, imfs for command menus
 */
void userfonts_(ifs,itfs,imfs)
long int *ifs,*itfs,*imfs;
{
 butn_fnt = (int) *ifs;	/* remember the button and graph text font size */
 disp_fnt = (int) *itfs;	/* dialogue and text feedback  */
 menu_fnt = (int) *imfs;	/* prefered menu font */
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


/* *************** Release display. *************** */
void winfin_()
{
  ipset(IPOFF);
/* << this might also be the place to free any loaded fonts >> */
  return;
}

/* **************  Erase whole screen. ************* */
void winclr_()
{
  XClearWindow(theDisp,win);
  return;
}

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

/*
 * copy from box to a Pixmap, assumes that the box and the Pixmap
 * are the same width & height
 */
void box_to_pix(from,frombox,to,tw,th) Pixmap *from, *to; box frombox; int tw,th; {
  int fromx,fromy,fromh,fromw,height,width;

  fromx = frombox.b_left; fromy = frombox.b_top;
  fromh = HEIGHT(frombox); fromw = WIDTH(frombox);
  height = th;
  width = tw;
  if(fromw>=width && fromh>=height){
    XCopyArea(theDisp,win,(Pixmap)to,theGC,fromx,fromy,(unsigned int)width,(unsigned int)height,0,0);
  } else {
    XSetTile(theDisp,theGC,(Pixmap)from);
    XSetFillStyle(theDisp,theGC,FillTiled);
    XFillRectangle(theDisp,(Pixmap)to,theGC,0,0,(unsigned int)fromw,(unsigned int)fromh);
  }
  return;
}

/*
 * copy from Pixmap to a box location, assumes that the box and the Pixmap
 * are the same width & height
 */
void pix_to_box(from,fw,fh,tobox,to) Pixmap *from, *to; box tobox; int fw, fh; {
  int th,tw;

  th = HEIGHT(tobox); tw = WIDTH(tobox);
  if(fw>=tw && fh>=th){
    XCopyArea(theDisp,(Pixmap)from,(Pixmap)to,theGC,0,0,(unsigned int)fw,(unsigned int)fh,tobox.b_left,tobox.b_top);
  } else {
    XSetTile(theDisp,theGC,(Pixmap)from);
    XSetFillStyle(theDisp,theGC,FillTiled);
    XFillRectangle(theDisp,(Pixmap)to,theGC,tobox.b_left,tobox.b_top,(unsigned int)tw,(unsigned int)th);
  }
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
 Pixmap exbit,logobit,underit; /* bitmap from data, pixmap of logo, area under */
 long int iupx,iupy;
 box gmenubx;
 int ilen,x_hot,y_hot,result,persist;
 unsigned int iwidth,iheight;
 char name2[80];
 FILE *bf;

 iupx = *lix; iupy = *liy; persist = (int) *itime;
/*
 * Terminate at fortran length, find actual string length and then reterminate.
 */
  f_to_c_l(name,&len,&ilen); strncpy(name2,name,(unsigned int)ilen); name2[ilen]='\0';
  if ((bf = fopen(name2,"r"))==NULL) {
    fprintf(stderr,"could not open bitmap file %s\n",name2);
    fclose(bf);
    return;
  }
/* Fill bitmap exbit from data file, make pixmap for under area and to hold transformed exbit data (logobit) */
/* Use XCopyPlane to transform exbit to logobit (seems to be required) */
 result = XReadBitmapFile(theDisp,(Pixmap)win,name2,&iwidth,&iheight,&exbit,&x_hot,&y_hot);
// fprintf(stderr,"result of XReadBitmapFile %d %d %d %ld %ld %d\n",result,iwidth,iheight,iupx,iupy,persist);
 if (result == BitmapFileInvalid) fprintf(stderr,"bitmap file %s invalid\n",name2);
 else if (result == BitmapOpenFailed) fprintf(stderr,"bitmap file %s cannot be opened\n",name2);
 else if (result == BitmapNoMemory) fprintf(stderr,"not enough bitmap memory\n");
 else if (result == BitmapSuccess) {
   underit = XCreatePixmap(theDisp,win,iwidth,iheight,dispDEEP);
   logobit = XCreatePixmap(theDisp,win,iwidth,iheight,dispDEEP);
   XCopyPlane(theDisp,(Pixmap)exbit,(Pixmap)logobit,theGC,0,0,iwidth,iheight,0,0,(unsigned long) 1);
   if ((iupy - (int) iheight) <= 0) {
     gmenubx.b_top = 10; gmenubx.b_bottom= 10 + (int) iheight;
   } else {
     gmenubx.b_top = (int) iupy - (int) iheight; gmenubx.b_bottom= (int) iupy;
   }
   gmenubx.b_left  = (int) iupx;  gmenubx.b_right = (int) iupx + iwidth;
/* Save area of gmenubx to underit then copy logbit to gmenubx area and flush display */
   box_to_pix((Pixmap)win,gmenubx,(Pixmap)underit,(int) iwidth,(int) iheight);
   XFlush(theDisp); /* force drawing  */
   XCopyArea(theDisp,(Pixmap)logobit,(Pixmap)win,theGC,0,0,iwidth,iheight,gmenubx.b_left,gmenubx.b_top);
   XFlush(theDisp); /* force drawing  */
   if(persist >= 10) {
     Timer(persist);
/* Copy saved underit back to gmenubx area, force drawing and then clear the bitmaps */
     XCopyArea(theDisp,(Pixmap)underit,(Pixmap)win,theGC,0,0,iwidth,iheight,gmenubx.b_left,gmenubx.b_top);
     XFlush(theDisp);
   }
   XFreePixmap(theDisp, underit);
   XFreePixmap(theDisp, exbit);
   XFreePixmap(theDisp, logobit);
 }
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
 Pixmap exbit,logobit,underit; /* bitmap from data, pixmap of logo, area under */
 long int ilreqx,ilreqy,ilreqwidth,ilreqheight;
 long int iupx,iupy;
 box gmenubx;
 int ilen,x_hot,y_hot,result;
 unsigned int iwidth,iheight;
 char name2[80];
 FILE *bf;
 ilreqx=*lreqx; ilreqy=*lreqy; ilreqwidth=*lreqwidth; ilreqheight=*lreqheight;
 iupx = *lix; iupy = *liy;
/*
 * Terminate at fortran length, find actual string length and then reterminate.
 */
  f_to_c_l(name,&len,&ilen); strncpy(name2,name,(unsigned int)ilen); name2[ilen]='\0';
  if ((bf = fopen(name2,"r"))==NULL) {
    fprintf(stderr,"could not open bitmap file %s\n",name2);
    fclose(bf);
    return;
  }
/* Fill bitmap exbit from data file, make pixmap for under area and to hold transformed exbit data (logobit) */
/* Use XCopyPlane to transform exbit to logobit (seems to be required) */
 result = XReadBitmapFile(theDisp,(Pixmap)win,name2,&iwidth,&iheight,&exbit,&x_hot,&y_hot);
 // fprintf(stderr,"result of XReadBitmapFile %d %u %u %ld %ld\n",result,iwidth,iheight,iupx,iupy);

 XSetForeground(theDisp,theGC,black);
 XSetBackground(theDisp,theGC,white);

/* there are several permutations of sizes of bitmap and copy origin and
   sizes of the saved area pixmap and destination origin that need to be
   sorted out.... */
/* if bitmap is smaller than region requested use bitmap size */
 if(iwidth < (unsigned int)ilreqwidth) ilreqwidth = (long int)iwidth;
 if(iheight < (unsigned int)ilreqheight) ilreqheight = (long int)iheight;

 // fprintf(stderr,"get region is %ld %ld %ld %ld\n",ilreqx,ilreqy,ilreqwidth,ilreqheight);
 if (result == BitmapFileInvalid) fprintf(stderr,"bitmap file %s invalid\n",name2);
 else if (result == BitmapOpenFailed) fprintf(stderr,"bitmap file %s cannot be opened\n",name2);
 else if (result == BitmapNoMemory) fprintf(stderr,"not enough bitmap memory\n");
 else if (result == BitmapSuccess) {
   underit = XCreatePixmap(theDisp,win,(unsigned int)ilreqwidth,(unsigned int)ilreqheight,dispDEEP);
   logobit = XCreatePixmap(theDisp,win,(unsigned int)ilreqwidth,(unsigned int)ilreqheight,dispDEEP);
   XCopyPlane(theDisp,(Pixmap)exbit,(Pixmap)logobit,theGC,(int)ilreqx,(int)ilreqy,(unsigned int)ilreqwidth,(unsigned int)ilreqheight,0,0,(unsigned long) 1);
   if ((iupy - (int) ilreqheight) <= 0) {
     gmenubx.b_top = 15; gmenubx.b_bottom= 15 + (int) ilreqheight;
   } else {
     gmenubx.b_top = (int) iupy - (int) ilreqheight; gmenubx.b_bottom= (int) iupy;
   }
   gmenubx.b_left  = (int) iupx;  gmenubx.b_right = (int) iupx + ilreqwidth;

/* Save area of gmenubx to underit then copy logbit to gmenubx area and flush display */
   box_to_pix((Pixmap)win,gmenubx,(Pixmap)underit,(int) ilreqwidth,(int) ilreqheight);
   XFlush(theDisp); /* force drawing  */
   XCopyArea(theDisp,(Pixmap)logobit,(Pixmap)win,theGC,0,0,(unsigned int)ilreqwidth,(unsigned int)ilreqheight,gmenubx.b_left,gmenubx.b_top);
   XFlush(theDisp); /* force drawing  */
   XFreePixmap(theDisp, underit);
   XFreePixmap(theDisp, exbit);
   XFreePixmap(theDisp, logobit);

/* return information on pixel corners of the bitmap on the screen */
  *boxulx=gmenubx.b_left; *boxuly=gmenubx.b_top;
  *boxlrx=gmenubx.b_left + ilreqwidth; *boxlry=gmenubx.b_top +ilreqheight;
 }
 fclose(bf);
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
 Pixmap exbit; /* bitmap from data file */
 long int ilreqx,ilreqy,ilreqwidth,ilreqheight;
 long int iupx,iupy;
 box gmenubx;
 int ilen,in,x_hot,y_hot,result;
 unsigned int iwidth,iheight;
 char name2[80];
 FILE *bf;
 ilreqx=*lreqx; ilreqy=*lreqy; ilreqwidth=*lreqwidth; ilreqheight=*lreqheight;
 iupx = *lix; iupy = *liy;
/*
 * Terminate at fortran length, find actual string length and then reterminate.
 */
  f_to_c_l(name,&len,&ilen); strncpy(name2,name,(unsigned int)ilen); name2[ilen]='\0';
  if ((bf = fopen(name2,"r"))==NULL) {
    fprintf(stderr,"could not open bitmap file %s\n",name2);
    fclose(bf);
    return;
  }
/* Fill bitmap exbit from data file, and if the result is ok find the positions it would take. */
 result = XReadBitmapFile(theDisp,(Pixmap)win,name2,&iwidth,&iheight,&exbit,&x_hot,&y_hot);
 // fprintf(stderr,"result of XReadBitmapFile %d %u %u %ld %ld\n",result,iwidth,iheight,iupx,iupy);

/* there are several permutations of sizes of bitmap and copy origin and
   sizes of the saved area pixmap and destination origin that need to be
   sorted out.... */
/* if bitmap is smaller than region requested use bitmap size */
 if(iwidth < (unsigned int)ilreqwidth) ilreqwidth = (long int)iwidth;
 if(iheight < (unsigned int)ilreqheight) ilreqheight = (long int)iheight;

 // fprintf(stderr,"get region is %ld %ld %ld %ld\n",ilreqx,ilreqy,ilreqwidth,ilreqheight);
 if (result == BitmapFileInvalid) fprintf(stderr,"bitmap file %s invalid\n",name2);
 else if (result == BitmapOpenFailed) fprintf(stderr,"bitmap file %s cannot be opened\n",name2);
 else if (result == BitmapNoMemory) fprintf(stderr,"not enough bitmap memory\n");
 else if (result == BitmapSuccess) {
   if ((iupy - (int) ilreqheight) <= 0) {
     gmenubx.b_top = 15; gmenubx.b_bottom= 15 + (int) ilreqheight;
   } else {
     gmenubx.b_top = (int) iupy - (int) ilreqheight; gmenubx.b_bottom= (int) iupy;
   }
   gmenubx.b_left  = (int) iupx;  gmenubx.b_right = (int) iupx + ilreqwidth;

   XFreePixmap(theDisp, exbit);

/* return information on pixel corners of the bitmap on the screen */
  *boxulx=gmenubx.b_left; *boxuly=gmenubx.b_top;
  *boxlrx=gmenubx.b_left + ilreqwidth; *boxlry=gmenubx.b_top +ilreqheight;
 }
 fclose(bf);
 return;
}

/* ********* Write a string beginning at pixel x and y to file. ******* */
void wstxptwwc_(x,y,buff,len)
long int *x, *y;       /* x y is the position of the string */
char *buff;
int  len;        /* len is length passed from fortran */
{
 int ilen;
 f_to_c_l(buff,&len,&ilen);

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}


/* ********* Write a string beginning at pixel x and y. ******* */
void wstxpt_(x,y,buff,len)
char *buff;
long int *x, *y;       /* x y is the position of the string */
int  len;        /* len is length passed from fortran */
{
 XftDraw *draw;
 int ix = (int) *x;
 int iy = (int) *y;
 int ilen;
 f_to_c_l(buff,&len,&ilen);

// Define local drawable for Xft font.
 draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
 XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
 XftDrawDestroy(draw);

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}

/* ********* textatxywwc_() write a string at pixel x y to file. ******* */
void textatxywwc_(x,y,buff,act,n,len)
long int *x, *y;  /* x y is the position of the string */
char *buff;
char *act;        /* single character passed for colour set */
long int *n;      /* colour index within the set */
int  len;         /* len is length passed from fortran */
{
/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
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
long int *x, *y;  /* x y is the position of the string */
char *buff;
char *act;        /* single character passed for colour set */
long int *n;      /* colour index within the set */
int  len;         /* len is length passed from fortran */
{
 XftDraw *draw;
 XftColor xft_current;
 unsigned long colid;	/* local X color id */
 long int xcolid;
 int ix = (int) *x;
 int iy = (int) *y;
 int ilen;
 int ic;
 ic = (int) *n;

 f_to_c_l(buff,&len,&ilen);
 draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* sets the current forground colour n depending on which active colour set being used */
 if(*act == 'g') {
    if (ic >= 0 && ic <= ngscale ) {
      XftDrawString8(draw, &xft_gscale[ic],fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == 'z') {
    if (ic >= 0 && ic <= izc ) {
      XftDrawString8(draw, &xft_zscale[ic],fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == 'c') {
    if (ic >= 0 && ic <= ncscale ) {
      XftDrawString8(draw, &xft_cscale[ic],fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == 'i') {
    if (ic >= 0 && ic <= ngr ) {	/* including black and white */
      if (ic == 0) XftDrawString8(draw, &xft_gmenuhl,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 1) XftDrawString8(draw, &xft_gmodbg,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 2) XftDrawString8(draw, &xft_gpopfr,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 3) XftDrawString8(draw, &xft_gfeedfr,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 4) XftDrawString8(draw, &xft_ginvert,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 5) XftDrawString8(draw, &xft_grey50,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 6) XftDrawString8(draw, &xft_black,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 7) XftDrawString8(draw, &xft_white,fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == '-') {
    XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
 }
 XftDrawDestroy(draw);
 XSetForeground(theDisp,theGC,fg);

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}

/* ********* textsizeatxywwc_() write to file version. ******* */
/* NOTE: different parameter list from X version */
void textsizeatxywwc_(x,y,buff,size,act,n,len)
long int *x, *y; /* x y is the position of the string */
char *buff;
long int *size;  /* font size indicator (see below) */
char *act;       /* single character passed for colour set */
long int *n;     /* colour index within the set */
int  len;        /* len is length passed from fortran */
{
 int ilen;
 f_to_c_l(buff,&len,&ilen);

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}

/* ********* textsizeatxy_() write a string at pixel x y in size colour act & n. *******
This version, as opposed to texteatxy, should be used when the string does not need to be
clipped. No preceding call to winfnt is required. This is appropriate where clipping is not
likely to be required e.g. drawing graphs. */
/* NOTE: different parameter list from X version */

void textsizeatxy_(x,y,buff,size,act,n,len)
long int *x, *y; /* x y is the position of the string */
char *buff;
long int *size;  /* font size indicator (see below) */
char *act;       /* single character passed for colour set */
long int *n;     /* colour index within the set */
int  len;        /* len is length passed from fortran */
{
 XftDraw *draw;
 long int fsize;
 long int saved_font;
 long int the_font;
 unsigned long colid;	/* local X color id */
 long int xcolid;
 int ix = (int) *x;
 int iy = (int) *y;
 int ilen;
 int ic;
 ic = (int) *n;
 fsize = *size;
 if(fsize == 0) the_font=4;
 if(fsize == 1) the_font=4;
 if(fsize == 2) the_font=5;
 if(fsize == 3) the_font=6;
 saved_font = current_font;	/* save current font  */
 winfnt_(&the_font);	        /* set to size        */

 f_to_c_l(buff,&len,&ilen);
 draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* sets the current forground colour n depending on which active colour set being used */
 if(*act == 'g') {
    if (ic >= 0 && ic <= ngscale ) {
      XftDrawString8(draw, &xft_gscale[ic],fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == 'z') {
    if (ic >= 0 && ic <= izc ) {
      XftDrawString8(draw, &xft_zscale[ic],fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == 'c') {
    if (ic >= 0 && ic <= ncscale ) {
      XftDrawString8(draw, &xft_cscale[ic],fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == 'i') {
    if (ic >= 0 && ic <= ngr ) {	/* including black and white */
      if (ic == 0) XftDrawString8(draw, &xft_gmenuhl,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 1) XftDrawString8(draw, &xft_gmodbg,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 2) XftDrawString8(draw, &xft_gpopfr,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 3) XftDrawString8(draw, &xft_gfeedfr,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 4) XftDrawString8(draw, &xft_ginvert,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 5) XftDrawString8(draw, &xft_grey50,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 6) XftDrawString8(draw, &xft_black,fst,ix,iy,(XftChar8 *) buff,ilen);
      if (ic == 7) XftDrawString8(draw, &xft_white,fst,ix,iy,(XftChar8 *) buff,ilen);
    } else {
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
    }
 } else if(*act == '-') {
    XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buff,ilen);
  }
 XftDrawDestroy(draw);
 XSetForeground(theDisp,theGC,fg);
 winfnt_(&saved_font);                     /* restore font */

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*wstxpt\n");
   fprintf(wwc,"%ld %ld\n",*x,*y);
   fprintf(wwc,"%s\n",buff);
 }
 return;
}

/* ********* Find width of string (buff) in current font. ******* */
void textpixwidth_(buff,pixelwidth,pixelheight,len)
char *buff;
long int *pixelwidth;	/* width of the string in pixels */
long int *pixelheight;	/* height of the string in pixels */
int  len;        /* len is length passed from fortran */
{
 int ilen;
 int vfw;
 XGlyphInfo info;

/* find number of characters in buff, load metrics for current font and
   the find the pixel width. */
 f_to_c_l(buff,&len,&ilen);
 XftTextExtents8(theDisp,fst,buff,ilen,&info);
 vfw = info.xOff;
 if (vfw > 1 ) *pixelwidth = (long int) vfw;
 if (vfw > 1 ) *pixelheight = (long int) f_height;
 // debug fprintf(stderr,"phrase %s is %d pixels wide\n",buff,vfw);

 return;
}

/* ************ Select a font **************** */
/* select one of the 8 fonts by its index, load it and update the graphic context.
 * for some reason font_0 has been cleared after initial jwinint call and so
 * it is refreshed. */
void winfnt_(n)
 long int *n;
{
 long int font_index;
 XGlyphInfo info;

 char *test = "_AiV_";  /* test string to get aggregate width for fix or proportionate fonts */
 int vfw,lt;
 int f_baseline;

 font_index = *n;  /* cast to local variable */
 if(font_index>=10) font_index=1;  /* in case of 64 bit huge number */

  if( font_index == 0 ) {
    t0=(char *) getenv("EFONT_0");
    if ((t0 == NULL) || (t0  == "") || (strncmp(t0,"    ",4) == 0)) {
      strncpy(font_0,"Ubuntu Mono,Monospace-8:medium",30); font_0[30]='\0';
    } else {
      strcpy(font_0,getenv("EFONT_0"));
    }
    fst = fst_0;
  } else if( font_index == 1 ) {
    fst = fst_1;
  } else if( font_index == 2 ) {
    fst = fst_2;
  } else if( font_index == 3 ) {
    fst = fst_3;
  } else if( font_index == 4 ) {
    fst = fst_4;
  } else if( font_index == 5 ) {
    fst = fst_5;
  } else if( font_index == 6 ) {
    fst = fst_6;
  } else if( font_index == 7 ) {
    fst = fst_7;
  }
  f_height = fst->ascent + fst->descent;
  f_baseline = fst->descent;
  current_font = font_index;
/* vfw is calculated as the average width within the test string. */
  lt=clnblnk(test);
  XftTextExtents8(theDisp,fst,test,lt,&info);
  vfw = (info.xOff/lt);
  if ( vfw > 1 ) f_width = vfw;
//  fprintf(stderr,"current font info: fh %d fb %d vfw %d fwidth %d \n",f_height,f_baseline,vfw,f_width);

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1) {
    fprintf(wwc,"*winfnt\n");
    fprintf(wwc,"%ld\n",current_font);
  }
  return;
}

/* ************ Return character width of text feedback for font **************** */
/* for for font n return how many characters will fit in text feedback (cw) as well
 * as how many lines (nlines). (return as long ints) */
void charsusingfnt_(n,cw,nlines)
 long int *n,*cw,*nlines;
{
/* XFontStruct  *tfst; */
 XftFont  *tfst;
 XGlyphInfo info;
 char *test = "_AiV_";
 int vfw,lt;
 int tf_height; /* tf_height is local font height in pixels */
 int tf_width;  /* tf_width is local font width in pixels */
 tfst = fst_0;  /* initial assumption */
  if( *n == 0 ) {
    t0=(char *) getenv("EFONT_0");
    if ((t0 == NULL) || (t0  == "") || (strncmp(t0,"    ",4) == 0)) {
      strncpy(font_0,"Ubuntu Mono,Monospace-8:medium",30);  font_0[30]='\0';
    } else {
      strcpy(font_0,getenv("EFONT_0"));
    }
    tfst = fst_0;
  } else if( *n == 1 ) {
    tfst = fst_1;
  } else if( *n == 2 ) {
    tfst = fst_2;
  } else if( *n == 3 ) {
    tfst = fst_3;
  } else if( *n == 4 ) {
    tfst = fst_4;
  } else if( *n == 5 ) {
    tfst = fst_5;
  } else if( *n == 6 ) {
    tfst = fst_6;
  } else if( *n == 7 ) {
    tfst = fst_7;
  }
  tf_height = tfst->ascent + tfst->descent;
/* vfw is calculated as the average width within the test string. */
  lt = strlen(test);
  XftTextExtents8(theDisp,tfst,test,lt,&info);
  vfw = (info.xOff/lt);
  tf_width = 1;
  if ( vfw > 1 ) tf_width = vfw;
  *nlines = (int) ((disp.b_bottom - disp.b_top) / (tf_height+1));
  *cw = ((disp.b_right - disp.b_left) / tf_width)-2;
//  fprintf(stderr,"current font info: fh %d fw %d vfw %d nlines %ld characters %ld \n",
//    tf_height,tf_width,vfw,*nlines,*cw);

  return;
}

/* ****** box drawing ***** */
/* flags indicate clearing and/or inverting of area and whether border is drawn */
void xbox(b,fgc,bgc,flags) box b; unsigned long fgc; unsigned long bgc; int flags;{
  int width = 1;
  unsigned int wid,hight;
  Bool exp = 1;

  hight = (unsigned int)(b.b_bottom-b.b_top);
  wid = (unsigned int)(b.b_right-b.b_left);
  if(flags & ~(BMEDGES|BMCLEAR|BMNOTALL|BMNOT|BMCLEARALL))
          fprintf(stderr,"unknown flag to xbox");
  if(WIDTH(b)<=0 || HEIGHT(b)<=0 ||
	   ((wid<=2 || hight<=2) && (flags&~(BMCLEAR|BMNOT))==0))
		return;	/* dont draw edges either! */
//  fprintf(stderr,"xbox fgc bgc is %ld %ld\n",fgc,bgc);
  if(flags&(BMCLEARALL|BMNOTALL)){
/* clear area, invert colours, fill, reset colours, draw outline */
    XClearArea(theDisp,win,b.b_left,b.b_top,(unsigned int)wid,(unsigned int)hight,exp);
    XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
    XSetForeground(theDisp,theGC, bgc);
    XSetBackground(theDisp,theGC, bgc);
    XFillRectangle(theDisp,win,theGC,b.b_left,b.b_top,(unsigned int)wid,(unsigned int)hight);
    XSetForeground(theDisp,theGC, fgc);
    XSetBackground(theDisp,theGC, bgc);
  }
  if(flags&(BMCLEAR|BMNOT)){
    XClearArea(theDisp,win,b.b_left+1,b.b_top+1,wid-1,hight-2,exp); /* clear inner box */
    XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
    XSetForeground(theDisp,theGC, bgc);
    XSetBackground(theDisp,theGC, bgc);
    XFillRectangle(theDisp,win,theGC,b.b_left+1,b.b_top+1,wid-1,hight-1);
    XSetForeground(theDisp,theGC, fgc);
    XSetBackground(theDisp,theGC, bgc);
  }
  if(flags&BMEDGES){
    XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter); /* set line style */
    XDrawRectangle(theDisp,win,theGC,b.b_left,b.b_top,wid,hight); /* draw bounding box */
  }
  XFlush(theDisp);
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

/* **************  Open feedback background box *************** */
/*
 Passed the character width of the main control menu, the number of
 lines of text to leave at the bottom for a dialogue box.
 dbx1 is the outer box (including axes) and viewbx is the image area.
*/
void feedbox_(menu_char,d_lines,gw,gh)
 long int	*menu_char,*d_lines,*gw,*gh;
{
  long int saved_font;
  int mf_width,label_ht,label_wid;
  int width = 1;
  unsigned int wid,hight;
  Bool exp = 1;

  saved_font = current_font;
  menu_offset = (int) *menu_char;    /* remember feedbox right character offset  */
  fbb_b_lines = (int) *d_lines;     /* remember feedbox bottom character margine */
  fbb.b_top   = 2;
  winfnt_(&butn_fnt); /* small font (used for continue text).  */
  label_ht = f_height+4;
  label_wid = f_width;
  winfnt_(&menu_fnt);  /* menu font to get right side of box.  */
  mf_width = f_width;
  winfnt_(&butn_fnt);  /* button font (used for dialogue text).  */

/*
 If d_lines is 0 then there is no dialogue box.
*/
/* XftTextExtents8 needed here */
  if (d_lines == 0){
    fbb.b_bottom= xrt_height - 5;
  } else {
     fbb.b_bottom= (int) xrt_height - ((f_height+4) * (*d_lines)) - 5;
  }
  fbb.b_left  = 2;
  fbb.b_right = (int) xrt_width - (mf_width * (*menu_char)) -8;
  hight = (unsigned int)(fbb.b_bottom-fbb.b_top);
  wid = (unsigned int)(fbb.b_right-fbb.b_left);

/* Clear area under the rectangle, set colours (gfeedfr), fill rectangle, re-set colours
 * and draw the bounding box.
 */
  XClearArea(theDisp,win,fbb.b_left,fbb.b_top,(unsigned int)wid,(unsigned int)hight,exp);
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
  XSetForeground(theDisp,theGC, gfeedfr);
  XSetBackground(theDisp,theGC, gfeedfr);
  XFillRectangle(theDisp,win,theGC,fbb.b_left,fbb.b_top,(unsigned int)wid,(unsigned int)hight);
  XSetForeground(theDisp,theGC, black);
  XSetBackground(theDisp,theGC, bg);
  XDrawRectangle(theDisp,win,theGC,fbb.b_left,fbb.b_top,wid,hight);

  *gh= fbb.b_bottom - fbb.b_top; /* feedback box pixel height  */
  *gw= fbb.b_right - fbb.b_left; /* feedback box pixel width */
/* debug  fprintf(stderr,"fg bg gfeedfr is %d %d %d\n",fg,bg,gfeedfr); */
  winfnt_(&saved_font);
  return;
} /* feedbox_ */

/* ****** scrollvh : draw scroll bars in the graphics windows ********** */
void scrollvh()
{
  long int iq;
  int height, offset, scrollset;
  float pv,tv,cv,ph,th,ch;

/* Draw scroll boxes */
  scrollv.b_left = dbx1.b_left;
  scrollv.b_right = dbx1.b_left + 9;
  scrollv.b_top = dbx1.b_top;
  scrollv.b_bottom = dbx1.b_bottom;
  xbox(scrollv,fg,white,BMCLEAR |BMEDGES);      /* draw box with edges  */

  scrollh.b_left = dbx1.b_left + 9;
  scrollh.b_right = dbx1.b_right;
  scrollh.b_top = dbx1.b_bottom-9;
  scrollh.b_bottom = dbx1.b_bottom;
  xbox(scrollh,fg,white,BMCLEAR |BMEDGES);      /* draw box with edges  */

/* query the graphical display about its current view extents and limits
this enables the size of the scroll bar to be set*/

/* Draw visible text box - make slightly narrower than scroll box */
  scrollbarv.b_left = scrollv.b_left + 1;
  scrollbarv.b_right = scrollv.b_right -1;

  scrollbarh.b_top = scrollh.b_top+1 ;
  scrollbarh.b_bottom = scrollh.b_bottom -1;

  gnwkquery_(&iq,&pv,&tv,&cv,&ph,&th,&ch);
  if(iq>0){
/* get vertical scroll bar parameters*/
  height=scrollv.b_bottom-scrollv.b_top-2;
  if(height<5) height=5;
  if(tv>pv&&pv>0.0){
    height=(int)((float)height*pv);
  }
  if(cv>0.0){
    offset=(int)(cv*(scrollv.b_bottom-scrollv.b_top-2));
    scrollset=(int)(0.5*height);
    scrollbarv.b_bottom=scrollv.b_bottom-1-offset+scrollset;
  }
  scrollbarv.b_top=scrollbarv.b_bottom-height;

/*get horizontal scroll bar parameters*/
  height=scrollh.b_right-scrollh.b_left-2;
  if(height<5) height=5;
  if(th>ph&&ph>0.0){
    height=(int)((float)height*ph);
  }
  if(ch>0.0){
    offset=(int)(ch*(scrollh.b_right-scrollh.b_left-2));
    scrollset=(int)(0.5*height);
    scrollbarh.b_left=scrollh.b_left+1+offset-scrollset;
  }
  scrollbarh.b_right=scrollbarh.b_left+height;
  }
  xbox(scrollbarh,fg,ginvert,BMCLEAR);      /* draw box */
  xbox(scrollbarv,fg,ginvert,BMCLEAR);      /* draw box */
  return;
}


/* **************  Open a 3D viewing box attributes to file *************** */
void win3dwwc_(menu_char,cl,cr,ct,cb,vl,vr,vt,vb,gw,gwht)
 long int	*menu_char,*gw,*gwht;
 long int	*cl,*cr,*ct,*cb;
 long int	*vl,*vr,*vt,*vb;
{
/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*win3d\n");
   fprintf(wwc,"%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld\n",
		*menu_char,*cl,*cr,*ct,*cb,*vl,*vr,*vt,*vb,*gw,*gwht);
 }
 return;
} /* win3dwwc_ */

/* **************  Open a 3D viewing box *************** */
/*
 Passed the character width of the main control menu (menu_char),
 the width of the left (cl), right (cr) inside margins in terms of
 number of characters with the butn_fnt font and the top (ct)and bottom
 (cb)inside margins in terms of lines of characters.
 Returns the pixel coord of the viewing box left, right, top, bottom as well
 as its overall pixel width & height.
 dbx1 is the outer box (including axes) and viewbx is the image area.
*/
void win3d_(menu_char,cl,cr,ct,cb,vl,vr,vt,vb,gw,gwht)
 long int	*menu_char,*gw,*gwht;
 long int	*cl,*cr,*ct,*cb;
 long int	*vl,*vr,*vt,*vb;
{
 long int saved_font;	/* nominal font used within rest of application.  */
 int label_ht,mf_width;     /* box label height and width of menu characters */
 int thegw,thegh;  /* local variable to take box dimensions from */

 saved_font = current_font;
 dbx1_avail = 1;        /* tell world that graphic box exists */
 c3dcl = (int) *cl; c3dcr = (int) *cr;   /* remember character offsets    */
 c3dct = (int) *ct; c3dcb = (int) *cb;
/*
 In case the text display window has been opened set this display
 slightly above it. Remember that the text display font may be smaller
 than the standard font.
*/
 winfnt_(&butn_fnt);  /* button font (used for label text).  */
 label_ht = f_height+6;
 winfnt_(&menu_fnt);  /* menu font to get right side of box.  */
 mf_width = f_width;
 winfnt_(&butn_fnt);  /* button font (used for graph text).  */
 dbx1.b_top = 8;
 if (disp_opened == 1){
   dbx1.b_bottom= disp.b_top - label_ht;
 } else {
   dbx1.b_bottom= xrt_height - ((f_height+4) * (dialogue_lines +1)) - 5;  /* !!!! 35 */
 }
 dbx1.b_left  = 8;
 dbx1.b_right = (int) xrt_width - (mf_width * (*menu_char)) -16;
 xbox(dbx1,fg,white,BMCLEAR |BMEDGES);        /* draw outer box with edges  */

 viewbx.b_top = dbx1.b_top + (f_height * (*ct));
 viewbx.b_bottom = dbx1.b_bottom - 9 - (f_height * (*cb));
 viewbx.b_left = dbx1.b_left + 9 + (f_width * (*cl));
 viewbx.b_right = dbx1.b_right - (f_width * (*cr));
 *vl=(long int) viewbx.b_left;                   /* pixel @ left   */
 *vr=(long int) viewbx.b_right;                  /* pixel @ right  */
 *vt=(long int) viewbx.b_top;                    /* pixel @ top    */
 *vb=(long int) viewbx.b_bottom;                 /* pixel @ bottom */
 xbox(viewbx,fg,white,BMCLEAR);	     /* clear viewing box */
 thegh= viewbx.b_bottom - viewbx.b_top;  /* viweing box pixel height  */
 thegw= viewbx.b_right -  viewbx.b_left;  /* viweing box pixel width */
 *gwht=(long int) thegh;   /* cast viweing box pixel height for return */
 *gw  =(long int) thegw;   /*  cast viweing box pixel width for return */

/* draw on the vertical and horizontal scroll bars */
  if(network_gpc)scrollvh();

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
   fprintf(wwc,"*win3d\n");
   fprintf(wwc,"%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld\n",
		*menu_char,*cl,*cr,*ct,*cb,*vl,*vr,*vt,*vb,*gw,*gwht);
 }
 return;
} /* win3d_ */

/* ******  Routine to clear view/3d display box. ********** */
void win3dclr_()
{
  xbox(dbx1,fg,white,BMCLEAR|BMEDGES);            /* clear graphic display box */

/* re draw on the vertical and horizontal scroll bars */
  if(network_gpc) scrollvh();
  return;
}

/* **************  Display text to file *************** */
/*
 Write viewtext attribures to file.
*/
void viewtextwwc_(msg,linep,side,size,len)
  char  *msg;              /* character string  */
  long int *linep, *side, *size;     /* position indicators */
  int len;                 /* length from f77   */
{
  int t_len;

  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*viewtext\n");
    fprintf(wwc,"%ld %ld %ld\n",*linep,*side,*size);
    fprintf(wwc,"%s\n",msg);
  }
  return;
} /* viewtextwwc  */

/* **************  Display text in viewing box *************** */
/*
 Given a string 'msg' and the 'line' where the string should be written
 and whether it should be left (side = 0), centered (side = 1), or
 right justified (side = 2) as well as the font size (0=small, 1 2 =medium,
 3=large and using a proportional font).
*/
void viewtext_(msg,linep,side,size,len)
  char  *msg;              /* character string  */
  int len;                 /* length from f77   */
  long int *linep, *side, *size;     /* position indicators */
{
  XftDraw *draw;
  int ix,iy,mid,t_len,fitpix;
  long int fsize;
  long int the_font;
  long int saved_font;
  XGlyphInfo info;
  box backing;	/* area under text to clear */

  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*viewtext\n");
    fprintf(wwc,"%ld %ld %ld\n",*linep,*side,*size);
    fprintf(wwc,"%s\n",msg);
  }

  fsize = *size;
  if(fsize == 0) the_font=4;
  if(fsize == 1) the_font=4;
  if(fsize == 2) the_font=5;
  if(fsize == 3) the_font=6;
  mid = dbx1.b_left + ((dbx1.b_right - dbx1.b_left)/2);
  saved_font = current_font;	/* save current font  */
  winfnt_(&the_font);	        /* set to size        */

  iy = dbx1.b_top + 3 + (f_height * *linep);
  if (*side == 0) {
      ix = dbx1.b_left + 7;
  } else if (*side == 1) {
      ix = mid - (f_width * t_len / 2);
  } else if (*side == 2) {
      ix = dbx1.b_right - (f_width * (t_len+1));
  } else {
      ix = dbx1.b_left + 7;
  }
  XftTextExtents8(theDisp,fst,msg,t_len,&info);
  fitpix = info.width;
  backing.b_bottom= iy; backing.b_top = backing.b_bottom - f_height;
  backing.b_left =ix; backing.b_right = backing.b_left + fitpix;
  xbox(backing,fg,white,BMCLEAR);            /* clear area under text */
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) msg,t_len);
  XftDrawDestroy(draw);
  winfnt_(&saved_font);                     /* restore font */
  return;
} /* viewtext  */

/* **************  Find position of display text in viewing box *************** */
/*
 Given a character position 'charpos' and the 'line' where the string should be written
 and return the pixel position that Xft should use.
*/
void findviewtext_(charposp,linep,size,irx,iry)
  long int *charposp, *linep, *size, *irx, *iry;     /* position indicators */
{
  int mid;
  long int fsize, charpos;
  long int saved_font;
  long int the_font;

  fsize = *size;
  if(fsize == 0) the_font=4;
  if(fsize == 1) the_font=4;
  if(fsize == 2) the_font=5;
  if(fsize == 3) the_font=6;
  charpos = *charposp;
  mid = dbx1.b_left + ((dbx1.b_right - dbx1.b_left)/2);
  saved_font = current_font;	/* save current font  */
  winfnt_(&the_font);	        /* set to size        */

  *iry = dbx1.b_top + 3 + (f_height * *linep);	/* y pixel at base of font */
  *irx = dbx1.b_left + 7 + (f_width * charpos);	/* x pixel at left of font (always have 7 pixels) */
  winfnt_(&saved_font);                     /* restore font */
  return;
} /* viewtext  */


/* **************  Display teklib tlabel to file ******* */
/*
 Write etlabel attributes to file.
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
  int t_len;

  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*etlabel\n");
    fprintf(wwc,"%f %f %ld %ld\n",*x,*y,*ipos,*size);
    fprintf(wwc,"%s\n",msg);
  }
  return;
} /* etlabelwwc */


/* **************  Display text as in old teklib tlabel ******* */
/*
 Given a string 'msg' and a reference position x,y in user units
 where the string should be written based on static variables from
 previous call to linescale.
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
  XftDraw *draw;
  float x1,y1;
  int ix,iy,mid,rig,p2,p0,t_len;
  long int fsize;
  long int saved_font,lix,liy;
  long int the_font;

  f_to_c_l(msg,&len,&t_len); if ( t_len < len ) msg[t_len] = '\0';

  if ( wwc_ok == 1) { /* If echo send parameters to wwc file */
    fprintf(wwc,"*etlabel\n");
    fprintf(wwc,"%f %f %ld %ld\n",*x,*y,*ipos,*size);
    fprintf(wwc,"%s\n",msg);
  }

  x1 = *x; y1 = *y; 
  fsize = *size;
  if(fsize == 0) the_font=4;
  if(fsize == 1) the_font=4;
  if(fsize == 2) the_font=5;
  if(fsize == 3) the_font=6;
  saved_font = current_font;	/* save current font  */
  winfnt_(&the_font);	/* set to size        */

  u2pixel_(&x1,&y1,&lix,&liy);  /* return pixel location of reference.   */
  ix = lix; iy = liy;           /* convert back to short int */

  rig = ix - (f_width * t_len);   /* right justified position */
  mid = ix - (f_width * t_len / 2); /* centred position */
  p2  = iy + f_height;                /* ref @ upper char */
  p0  = iy + (f_height / 2);          /* ref @ centred char */

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  if (*ipos == 0) {
      XftDrawString8(draw, &xft_color,fst,mid,p0,(XftChar8 *) msg,t_len);
  } else if (*ipos == 1) {
      XftDrawString8(draw, &xft_color,fst,rig,p0,(XftChar8 *) msg,t_len);
  } else if (*ipos == 2) {
      XftDrawString8(draw, &xft_color,fst,mid,p2,(XftChar8 *) msg,t_len);
  } else if (*ipos == 3) {
      XftDrawString8(draw, &xft_color,fst,ix,p0,(XftChar8 *) msg,t_len);
  } else if (*ipos == 4) {
      XftDrawString8(draw, &xft_color,fst,mid,iy,(XftChar8 *) msg,t_len);
  }
  XftDrawDestroy(draw);
  winfnt_(&saved_font); /* restore font */
  return;
} /* etlabel */

/* ************* force X buffer to flush ********* */
void forceflush_()
{
  XFlush(theDisp);
  return;
}

/* ************* pause_for_milliseconds ********* */
void Timer(msec) int msec;
{
#ifdef MINGW
#include <windows.h>
#endif 
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

/* ************* pause_for_milliseconds ********* */
void pausems_(msec)   /* from xvmisc.c */
 long int  *msec;
{
#ifdef MINGW
#include <windows.h>
#endif 
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

/* ************* pause_for_seconds ********* */
void pauses_(is)
  long int *is;
{
#ifdef MINGW
#include <windows.h>
#endif 
  int i;
  i = (int) *is;
#ifdef MINGW
/*  Sleep((unsigned int) msecond * 1000); */
#else
  sleep((unsigned int) i );
#endif
  return;
}

/* internal line drawing functions and functions callable from fortran */

/* ***** draw a line of width 1 between two points **** */
void drawswl(xa,ya,xb,yb)
  int xa,ya,xb,yb;
{
  int width = 1;
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
  XDrawLine(theDisp,win,theGC,xa,ya,xb,yb);
  return;
}


/* *************** ESRU symbol drawing routine to file *************** */
/*
 Write esymbol attributes to file.
*/
void esymbolwwc_(long int* x,long int* y,long int* sym,long int* size)
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*esymbol\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x,*y,*sym,*size);
  }
  return;
}


/* *************** ESRU symbol drawing routine. *************** */
/*
 esymbol is passed a pixel coord, a symbol index, and a size.
 currently there are 38 symbols.
*/
void esymbol_(long int* x,long int* y,long int* sym,long int* size)
{
  int isym,isize,ix,iy,width;
  XPoint p[12];
  isym = (int) *sym;
  isize = (int) *size;
  ix = (int) *x;
  iy = (int) *y;
  width = 1;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*esymbol\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x,*y,*sym,*size);
  }
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
/*
      p[0].x=; p[0].y=; p[1].x=; p[1].y=;
      p[2].x=; p[2].y=; p[3].x=; p[3].y=;
      p[4].x=; p[4].y=; p[5].x=; p[5].y=;
*/
  switch ( isym ) {    /* pick the appropriate symbol */
    case 0 :	       /* box 4 pixels wide and high */
      p[0].x=ix-2; p[0].y=iy-2; p[1].x=ix+2; p[1].y=iy-2;
      p[2].x=ix+2; p[2].y=iy+2; p[3].x=ix-2; p[3].y=iy+2;
      p[4].x=ix-2; p[4].y=iy-2;
      XDrawLines(theDisp,win,theGC,p,5,CoordModeOrigin); break;
    case 1 :  		/* triangle p[].x= ; p[].y= ; */
      p[0].x=ix; p[0].y=iy-3; p[1].x=ix+2; p[1].y=iy+2;
      p[2].x=ix-2; p[2].y=iy+2; p[3].x=ix; p[3].y=iy-3;
      XDrawLines(theDisp,win,theGC,p,4,CoordModeOrigin); break;
    case 2 : 		/* upside down triangle */
      p[0].x=ix; p[0].y=iy+3; p[1].x=ix+2; p[1].y=iy-2;
      p[2].x=ix-2; p[2].y=iy-2; p[3].x=ix; p[3].y=iy+3;
      XDrawLines(theDisp,win,theGC,p,4,CoordModeOrigin); break;
    case 3 : 		/* diamond */
      p[0].x=ix; p[0].y=iy-3; p[1].x=ix+3; p[1].y=iy;
      p[2].x=ix; p[2].y=iy+3; p[3].x=ix-3; p[3].y=iy;
      p[4].x=ix; p[4].y=iy-3; p[5].x=ix; p[5].y=iy-3;
      XDrawLines(theDisp,win,theGC,p,6,CoordModeOrigin); break;
    case 4 : 		/* lower right triangle */
      p[0].x=ix-3; p[0].y=iy+2; p[1].x=ix+2; p[1].y=iy+2;
      p[2].x=ix+2; p[2].y=iy-3; p[3].x=ix-3; p[3].y=iy+2;
      XDrawLines(theDisp,win,theGC,p,4,CoordModeOrigin); break;
    case 5 : 		/* lower left triangle */
      p[0].x=ix-2; p[0].y=iy+2; p[1].x=ix+3; p[1].y=iy+2;
      p[2].x=ix-2; p[2].y=iy-3; p[3].x=ix-2; p[3].y=iy+2;
      XDrawLines(theDisp,win,theGC,p,4,CoordModeOrigin); break;
    case 6 : 		/* upper left triangle */
      p[0].x=ix-2; p[0].y=iy+3; p[1].x=ix+3; p[1].y=iy-2;
      p[2].x=ix-2; p[2].y=iy-2; p[3].x=ix-2; p[3].y=iy+3;
      XDrawLines(theDisp,win,theGC,p,4,CoordModeOrigin); break;
    case 7 : 		/* upper right triangle */
      p[0].x=ix+2; p[0].y=iy+3; p[1].x=ix+2; p[1].y=iy-2;
      p[2].x=ix-3; p[2].y=iy-2; p[3].x=ix+2; p[3].y=iy+3;
      XDrawLines(theDisp,win,theGC,p,4,CoordModeOrigin); break;
    case 8 : 		/* size 1 = dot 2 pix wide, 3 pix high */
      if(isize==1) {
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
      XDrawLines(theDisp,win,theGC,p,3,CoordModeOrigin); break;
    case 13 : 		/* v down arrow */
      p[0].x=ix-2; p[0].y=iy-2; p[1].x=ix; p[1].y=iy+2;
      p[2].x=ix+3; p[2].y=iy-3;
      XDrawLines(theDisp,win,theGC,p,3,CoordModeOrigin); break;
    case 14 : 		/* > left arrow */
      p[0].x=ix-2; p[0].y=iy+2; p[1].x=ix+2; p[1].y=iy;
      p[2].x=ix-3; p[2].y=iy-3;
      XDrawLines(theDisp,win,theGC,p,3,CoordModeOrigin); break;
    case 15 : 		/* < right arrow */
      p[0].x=ix+2; p[0].y=iy+2; p[1].x=ix-2; p[1].y=iy;
      p[2].x=ix+3; p[2].y=iy-3;
      XDrawLines(theDisp,win,theGC,p,3,CoordModeOrigin); break;
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

/* **************  Open a query box *************** */
/*
  Standard query box for dialog that needs one. Takes text to display
  character width of box, pixels for lower left corner.
*/
void qbox_(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act){
/* where: *msg is text in box,
 *        msglen,asklen character width of the text and input box
 *        *b_bottom, *b_left are pixel at lower left of box (supplied)
 *        act is the action `-` nothing, `!` blink it. */

  XGlyphInfo info;
  XftDraw *draw;
  int lm1;		/* local string lengths */
  int  bottom, left;	/* pixel at lower left of box (supplied) */
  long int saved_font;
  int vfw;      /* pixels width for the box */
  int padding;  /* character difference between msglen and asklen */

  bottom = *b_bottom; left = *b_left;
  lm1=msglen; padding=(asklen - msglen);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

// Use XftTextExtents8 to get the actual pixels needed for the string.
  vfw=0;
  XftTextExtents8(theDisp,fst,msg,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff + (padding * f_width);  /* impose requested additional space */
//  fprintf(stderr,"qbox msg %s lm1 %d asklen %d vwf %d f_width %d asktimesfw %d\n",msg,lm1,asklen,vfw,f_width,(asklen * f_width));
  if( (asklen * f_width) > vfw ) vfw = asklen * f_width;
//  fprintf(stderr,"qbox new vfw is %d\n",vfw);

  querb.b_top = bottom - (f_height + 6);
  querb.b_bottom = bottom -2;
  querb.b_left = left;
  querb.b_right = querb.b_left + vfw;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  if(act == '-') {
    xbox(querb,fg,white,BMCLEAR |BMEDGES);   /* draw querry box with edges  */
    XftDrawString8(draw, &xft_color,fst,querb.b_left+3,querb.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if(act == '!') {
    xbox(querb,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
    XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
    XftDrawString8(draw, &xft_color,fst,querb.b_left+3,querb.b_bottom-3,(XftChar8 *) msg,lm1);
    XFlush(theDisp);
    XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
    Timer(200);
    xbox(querb,fg,white, BMCLEAR | BMEDGES);             /* clear box */
    XftDrawString8(draw, &xft_color,fst,querb.b_left+3,querb.b_bottom-3,(XftChar8 *) msg,lm1);
  }
  XFlush(theDisp);  /* added to force draw */
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* qbox_ */

/* **************  Open a defaults box *************** */
/*
  Standard defaults box for dialog that needs one. Takes text to display
  character width of box, pixels for lower left corner.
*/
void dbox(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act){
/* where: *msg is text in box,
 *        msglen,asklen are character width of the text and input box
 *        *b_bottom, *b_left are pixel at lower left of box (supplied)
 *        act is the action `-` nothing, `!` blink it. */

  XGlyphInfo info;
  XftDraw *draw;
  int lm1;		/* local string lengths found by test  */
  int  bottom, left;	/* pixel at lower left of box (supplied) */
  long int saved_font;
  int vfw;      /* pixels width for the box */
  int padding;  /* character difference between msglen and asklen */

  bottom = *b_bottom; left = *b_left;
  lm1=msglen; padding=(asklen - msglen);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

// Use XftTextExtents8 to get the actual pixels needed for the string.
  vfw=0;
  XftTextExtents8(theDisp,fst,msg,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff + (padding * f_width);  /* impose requested additional space */
//  fprintf(stderr,"dbox msg %s lm1 %d asklen %d vwf %d f_width %d asktimesfw %d\n",msg,lm1,asklen,vfw,f_width,(asklen * f_width));
  if( (asklen * f_width) > vfw ) vfw = asklen * f_width;
//  fprintf(stderr,"dbox new vfw is %d\n",vfw);

  defb.b_top = bottom - (f_height + 6);
  defb.b_bottom = bottom -2;
  defb.b_left = left;
//  defb.b_right = defb.b_left + (asklen * f_width);
  defb.b_right = defb.b_left + vfw;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  if(act == '-') {
    xbox(defb,fg,white,BMCLEAR |BMEDGES);   /* draw querry box with edges  */
    XftDrawString8(draw, &xft_color,fst,defb.b_left+3,defb.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if (act == '!') {
    xbox(defb,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);       /* invert box */
    XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
    XftDrawString8(draw, &xft_color,fst,defb.b_left+3,defb.b_bottom-3,(XftChar8 *) msg,lm1);
    XFlush(theDisp);
    XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
    Timer(200);
    xbox(defb,fg,white, BMCLEAR | BMEDGES);              /* clear box */
    XftDrawString8(draw, &xft_color,fst,defb.b_left+3,defb.b_bottom-3,(XftChar8 *) msg,lm1);
  }
  XFlush(theDisp);  /* added to force draw */
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* dbox */

/* **************  Open an ok box *************** */
/*
  Standard ok confirm box for dialog that needs one. Takes text to display
  character width of box, pixels for lower left corner.
*/
void okbox(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act){
/* where: *msg is text in box,
 *        msglen,asklen are character width of the text and input box
 *        *b_bottom, *b_left are pixel at lower left of box (supplied)
 *        act is the action `-` nothing, `!` blink it. */

  XGlyphInfo info;
  XftDraw *draw;
  int lm1;		/* local string lengths  */
  int  bottom, left;	/* pixel at lower left of box (supplied) */
  long int saved_font;
  int vfw;      /* pixels width for the box */
  int padding;  /* character difference between msglen and asklen */

  bottom = *b_bottom; left = *b_left;
  lm1=msglen;  padding=(asklen - msglen);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

// Use XftTextExtents8 to get the actual pixels needed for the string.
  vfw=0;
  XftTextExtents8(theDisp,fst,msg,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff + (padding * f_width);  /* impose requested additional space */
//  fprintf(stderr,"okbox msg %s lm1 %d asklen %d vwf %d f_width %d asktimesfw %d\n",msg,lm1,asklen,vfw,f_width,(asklen * f_width));
  if( (asklen * f_width) > vfw ) vfw = asklen * f_width;
//  fprintf(stderr,"okbox new vfw is %d\n",vfw);

  okb.b_top = bottom - (f_height + 6);
  okb.b_bottom = bottom -2;
  okb.b_left = left;
//  okb.b_right = okb.b_left + (asklen * f_width);
  okb.b_right = okb.b_left + vfw;

// Define local drawable for Xft font.
   draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  if(act == '-') {
    xbox(okb,fg,white,BMCLEAR |BMEDGES);   /* draw querry box with edges  */
    XftDrawString8(draw, &xft_color,fst,okb.b_left+3,okb.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if (act == '!') {
    xbox(okb,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);        /* invert box */
    XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
    XftDrawString8(draw, &xft_color,fst,okb.b_left+3,okb.b_bottom-3,(XftChar8 *) msg,lm1);
    XFlush(theDisp);
    XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
    Timer(200);
    xbox(okb,fg,white, BMCLEAR | BMEDGES);               /* clear box */
    XftDrawString8(draw, &xft_color,fst,okb.b_left+3,okb.b_bottom-3,(XftChar8 *) msg,lm1);
  }
  XFlush(theDisp);  /* added to force draw */
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* okbox */

/* *************** ESRU constrained floating pop-up menu. *************** */
/*
 This function takes a static list of selections and displays it in a box
 and waits for the user to click a mouse to select an item. No help or
 paging supported.
*/
#define OFFEND		(-1)	/* no item selected */

void epopup_(titleptr,	/* title for menu                   */
  listptr,		/* static list                      */
  impx,impy,		/* menu position (upper right)      */
  ino			/* selected item number: if ino<0 dont show menu */
  )
char	*titleptr;
char	**listptr;
long int *impx,*impy;
int *ino;
{
/* Local variables   */
  XEvent event;
  XWindowAttributes wa;
  Pixmap under;		        /* to save image under help box  */
  XftDraw *draw;
  int   iwth,ilen,num,l;	/* menu character width to print and length of list    */
  long int saved_font, use_font, changed_font;
  int   mob_height, mob_width,mib_height,mib_width,xb,yb,x,y,i,iy,lt1,lineheight;
  int   u_height, u_width;      /* size of the under pixmap */
  int   menu_height;   /* pixel height for text of menu */
  box	menubx, gmenubx;	/* menu display, outer box */
  box   hl_box;       /* for inverting box around text */
  Bool exp = 1;
  int	no_valid_event,config_altered;
  unsigned int start_height,start_width;
  long int popup_font = 5;  /* one up from smallest proportional font */

  ilen = 0; iwth = 0;
  for(num=0;listptr[num]!=NULLPTR(char);num++){
    if((l=(int)strlen(listptr[num]))>ilen)iwth = l;
  }
  ilen = num;
  if(ilen == 0){	/* don't bother if no list */
    *ino = -1;
    if(XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
    return;
  }
/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width; config_altered = 0;

  xb = (int) *impx;  yb = (int) *impy;
  lt1 = (int) strlen(titleptr);  /* width of title */
  changed_font = 0;
  saved_font = use_font = current_font; /* save existing font  */
  if ( use_font != popup_font ) { winfnt_(&popup_font); use_font = popup_font; changed_font = 1; }

/*  Check if the text will fit within window, if not down-size until it does.*/
  menu_height = (ilen+1)*(f_height+2);

  /* having established a font, figure out heights and widths of outer and inner box */
  mib_height = (ilen+1)*(f_height+2);   /* inner pixel height */
  mob_height = mib_height + (5 + (f_height + 8));  /* include space for title  */
  mib_width = (iwth*f_width);	/* inner pixel width */
  mob_width = mib_width +20;	/* box slightly wider than longest line */
/*
 If a specific box starting point has been passed then try to honor
 this otherwise place it slightly above the dialogue box.
*/
  if (xb == 0) {
    xb= xrt_width - (mob_width + 2);  yb = mob_height + 2;
  }

/* Create the menu box. */
  gmenubx.b_top   = yb - mob_height;  gmenubx.b_bottom= yb;
  gmenubx.b_left  = xb;  gmenubx.b_right = xb + mob_width;
  menubx.b_top   = gmenubx.b_top + (f_height + 8);  menubx.b_bottom= gmenubx.b_bottom - 5;
  menubx.b_left  = gmenubx.b_left + 5;  menubx.b_right = gmenubx.b_right - 5;
  hl_box.b_left = menubx.b_left +2;  hl_box.b_right = menubx.b_right -2;
  lineheight = (menubx.b_bottom - menubx.b_top - 3) / ilen;

  u_width = WIDTH(gmenubx); u_height = HEIGHT(gmenubx); /* width & height area to save */
  under = XCreatePixmap(theDisp,win,(unsigned int) u_width,(unsigned int) u_height,dispDEEP);
  box_to_pix(win,gmenubx,under,u_width,u_height);   /* save rect under gmenubx to under */

  xbox(gmenubx,fg,gpopfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
  xbox(gmenubx,fg,bg,BMEDGES);
  xbox(menubx,fg,white,BMCLEAR|BMEDGES);	/* draw menu display box  */

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,menubx.b_top-5,(XftChar8 *) titleptr,lt1);

/* display lines of text.  */
  for ( i = 0; i < ilen; i++ ) {
    iy = menubx.b_top + ((i + 1) * (f_height+2)) + 5;
    XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) listptr[i],iwth-1);
  }
  XFlush(theDisp); /* force drawing of menu text */
  no_valid_event = TRUE;
  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case Expose:
        break;
      case VisibilityNotify:
/* debug  fprintf(stderr,"epopup: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(gmenubx,fg,gpopfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
          xbox(gmenubx,fg,bg,BMEDGES);
          xbox(menubx,fg,white,BMCLEAR|BMEDGES);	/* draw menu display box  */
          XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,menubx.b_top-5,(XftChar8 *) titleptr,lt1);

/* re-display lines of text.  */
          for ( i = 0; i < ilen; i++ ) {
            iy = menubx.b_top + ((i + 1) * (f_height+2)) + 5;
            XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) listptr[i],iwth-1);
          }
          XFlush(theDisp);
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug fprintf(stderr,"epopup detected configure event\n");  */
          config_altered = 1;
          refreshenv_();
          xbox(gmenubx,fg,gpopfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
          xbox(gmenubx,fg,bg,BMEDGES);
          xbox(menubx,fg,white,BMCLEAR|BMEDGES);	/* draw menu display box  */
          XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,menubx.b_top-5,(XftChar8 *) titleptr,lt1);

/* re-display lines of text.  */
          for ( i = 0; i < ilen; i++ ) {
            iy = menubx.b_top + ((i + 1) * (f_height+2)) + 5;
            XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) listptr[i],iwth-1);
          }
          XFlush(theDisp);
        }
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        if (xboxinside(menubx,x,y)){

/* User has clicked within the bounds of a item, confirm by hilighting item. */
	   no_valid_event = FALSE;
           i = (( y - (menubx.b_top + 5)) / lineheight);
           iy = menubx.b_top + ((i + 1) * (f_height+2)) + 5;
           hl_box.b_bottom = iy + 3;
           hl_box.b_top = hl_box.b_bottom - (f_height+4);
/* debug   fprintf(stderr,"item %s %d %d %d %d\n",listptr[i],i,iy,hl_box.b_bottom,hl_box.b_top);  */
           xbox(hl_box,fg,ginvert, BMCLEAR | BMNOT );        /* grey item */
           XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
           XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) listptr[i],iwth-1);
           XFlush(theDisp);
           XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
           Timer(300);
           xbox(menubx,fg,white,BMEDGES|BMCLEAR);	/* clear menu box */
           *ino = i;  /* return index (based on list starting at zero */
   	   break;
         }
      case KeyPress:	/* (XKeyEvent)&ev */
	XNextEvent (theDisp,&event);	/* flush event */
        break;
    }
  }

  XClearArea(theDisp,win,gmenubx.b_left,gmenubx.b_top,(unsigned int) u_width,(unsigned int) u_height,exp);
/* if root window size unchanged then restore the under pixmap otherwise refresh the display */
  if(config_altered == 0)pix_to_box(under,u_width,u_height,gmenubx,win);

  XFreePixmap(theDisp, under);
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  if (changed_font == 1) winfnt_(&saved_font);  /* Restore original font.  */
  XFlush(theDisp); /* added to force draw */
  XftDrawDestroy(draw);
  if(config_altered == 1) refreshenv_();
  return;
}

/* **************  Work with a do something box *************** */
/*
  Standard redraw and invocation box. Takes text to display
  character width of box, pixels for lower left corner.
*/
void doitbox(box dobox,char* msg,int msglen,int asklen,long int* sav_font,long int* use_font,
	int* b_bottom,int* b_left,char* topic,char act){
/* where:  dobox is the box to do it in,
 *         *msg is the text in box,
 *         *topic which topic this button supports,
 *         msglen,asklen character width of the text and input box,
 *         *sav_font, *use_font font in current use, font to use within box,
 *         *b_bottom, *b_left pixel at lower left of box (supplied),
 *         act action to take (- is draw, ! is hilight and do  */
  XGlyphInfo info;
  XftDraw *draw;
  int lm1;	/* local string lengths  */
  int bottom, left;	/* pixel at lower left of box (supplied) */
  long int s_font, u_font;	/* font in current use, font to use within box,  */
  long int  last,new,lastm,lastb; 	/* fonts for updating */
  long int avail_wire;	/* current value of wire_avail to pass to fortran. */
  long int avail_cpw;	/* current value of copyright to pass to fortran. */
  long int iupx,iupy;	/* position of capture popup */
  int choice;	/* initial popup index */
  int vfw;      /* pixels width for the box */
  int padding;  /* character difference between msglen and asklen */

  bottom = *b_bottom; left = *b_left;
  s_font = *sav_font, u_font = *use_font;
  lm1=msglen; padding=(asklen - msglen);
  if (s_font != u_font) winfnt_(&u_font);

// Use XftTextExtents8 to get the actual pixels needed for the string.
  vfw=0;
  XftTextExtents8(theDisp,fst,msg,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff + (padding * f_width);  /* impose requested additional space */
//  fprintf(stderr,"doitbox msg %s lm1 %d asklen %d vwf %d f_width %d asktimesfw %d\n",msg,lm1,asklen,vfw,f_width,(asklen * f_width));
  if( (asklen * f_width) > vfw ) vfw = asklen * f_width;
//  fprintf(stderr,"doitbox new vfw is %d\n",vfw);

  dobox.b_bottom = bottom - 2;  dobox.b_top = bottom - (f_height + 3);
/*  dobox.b_left = left;  dobox.b_right = dobox.b_left + (asklen * f_width); */
  dobox.b_left = left;  dobox.b_right = dobox.b_left + vfw;
  if (strncmp(topic, "wire", 4) == 0) { /* assign box definition to relevant structure */
    wire = dobox;
  } else if (strncmp(topic, "azi", 3) == 0) {
    azi = dobox;
  } else if (strncmp(topic, "elev", 4) == 0) {
    elev = dobox;
  } else if (strncmp(topic, "capture", 7) == 0) {
    capture = dobox;
  } else if (strncmp(topic, "captext", 7) == 0) {
    captext = dobox;
  } else if (strncmp(topic, "setup", 5) == 0) {
    setup = dobox;
  } else if (strncmp(topic, "copyright", 9) == 0) {
    cpw = dobox;
  }

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  if(act == '-') {
    xbox(dobox,fg,white,BMCLEAR |BMEDGES);   /* draw box with edges & text */
    XftDrawString8(draw, &xft_color,fst,dobox.b_left+3,dobox.b_bottom-2,(XftChar8 *) msg,lm1);
  } else if (act == '!') {
    xbox(dobox,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);        /* invert */
    XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
    XftDrawString8(draw, &xft_color,fst,dobox.b_left+3,dobox.b_bottom-2,(XftChar8 *) msg,lm1);
    XFlush(theDisp);
    XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
    if (s_font != u_font) winfnt_(&s_font);  /* restore std font during action */

    if (strncmp(topic, "wire", 4) == 0) {
      Timer(200);
      avail_wire = wire_avail;
      wirepk_(&avail_wire);  /* Deal with user selection of wireframe control  */
    } else if (strncmp(topic, "azi", 3) == 0) {
      Timer(200);
    } else if (strncmp(topic, "elev", 4) == 0) {
      Timer(200);
    } else if (strncmp(topic, "capture", 7) == 0) {
/*
 * Deal with user selection of capture pop-up memu. The tasks will be done
 * based on information external to the application (in the user's .esprc file).
 */
        profgrdump_(); /* use subroutine in esrsu_cut_lib.f to invoke graphics capture */
    } else if (strncmp(topic, "captext", 7) == 0) {
        proftxdump_();	/* use subroutine in esru_cut_lib.f to process text buffer. */
    } else if (strncmp(topic, "setup", 5) == 0) {
       choice = -1;
       iupx = disp.b_left + (disp.b_right - disp.b_left)/2;
       iupy = dbx1.b_bottom -25; 
       if (iupy < 200) iupy=200;  /* keep it disapearing off the top */
       epopup_("font options",fontmenu,&iupx,&iupy,&choice);

       last=disp_fnt;
       lastm=menu_fnt;
       lastb=butn_fnt;

       switch(choice){
         case 0:  /* also this impacts menuchw */
           if(menu_fnt >0 && menu_fnt <= 3) new = menu_fnt-1;
           if(menu_fnt >4 && menu_fnt <= 7) new = menu_fnt-1;
           if(menu_fnt != new) { menu_fnt = new; refreshenv_(); }
           break;
         case 1:  /* also this impacts menuchw */
           if(menu_fnt >=0 && menu_fnt < 3) new = menu_fnt+1;
           if(menu_fnt >3 && menu_fnt < 7) new = menu_fnt+1;
           if(menu_fnt != new) { menu_fnt = new; refreshenv_(); }
           break;
         case 2:
           if(disp_fnt >0 && disp_fnt <= 3) new = disp_fnt-1;
           if(disp_fnt >4 && disp_fnt <= 7) new = disp_fnt-1;
           if (new != disp_fnt) { disp_fnt = new; winfnt_(&disp_fnt); }
           disp_lines = (int) ((disp.b_bottom - disp.b_top) / (f_height+1));
           refreshenv_();
           break;
         case 3:
           if(disp_fnt >=0 && disp_fnt < 3) new = disp_fnt+1;
           if(disp_fnt >3 && disp_fnt < 7) new = disp_fnt+1;
           if (new != disp_fnt) { disp_fnt = new; winfnt_(&disp_fnt); }
           disp_lines = (int) ((disp.b_bottom - disp.b_top) / (f_height+1));
           refreshenv_();
           break;
         case 4:
           if(butn_fnt >0 && butn_fnt <= 3) new = butn_fnt-1;
           if(butn_fnt >4 && butn_fnt <= 7) new = butn_fnt-1;
           if(butn_fnt != new) { butn_fnt = new; refreshenv_(); }
           break;
         case 5:
           if(butn_fnt >=0 && butn_fnt < 3) new = butn_fnt+1;
           if(butn_fnt >3 && butn_fnt < 7) new = butn_fnt+1;
           if(butn_fnt != new) { butn_fnt = new; refreshenv_(); }
           break;
         case 6:  /* re-establish the default fonts for the application */
           if(menu_fnt >0 && menu_fnt <= 3) new = d_menu_fnt;
           if(menu_fnt >4 && menu_fnt <= 7) new = d_menu_fnt;
           if(menu_fnt != new) menu_fnt = new;
           if(disp_fnt >=0 && disp_fnt < 3) new = d_disp_fnt;
           if(disp_fnt >3 && disp_fnt < 7) new = d_disp_fnt;
           if (new != disp_fnt) { disp_fnt = new; winfnt_(&disp_fnt); }
           disp_lines = (int) ((disp.b_bottom - disp.b_top) / (f_height+1));
           if(butn_fnt >=0 && butn_fnt < 3) new = d_butn_fnt;
           if(butn_fnt >3 && butn_fnt < 7) new = d_butn_fnt;
           if(butn_fnt != new) butn_fnt = new;
           refreshenv_();
           break;
         case 7:
           break;
         default: break;
       }

    } else if (strncmp(topic, "copyright", 9) == 0) {
      avail_cpw = cpw_avail;
      cpwpk_(&avail_cpw);
    } else if (strncmp(topic, "-", 1) == 0) {
      Timer(200);
    }

    if (s_font != u_font) winfnt_(&u_font);
    xbox(dobox,fg,white, BMCLEAR | BMEDGES);               /* clear box */
    XftDrawString8(draw, &xft_color,fst,dobox.b_left+3,dobox.b_bottom-3,(XftChar8 *) msg,lm1);
  }
  XFlush(theDisp);
  XftDrawDestroy(draw);
  if (s_font != u_font) winfnt_(&s_font);  /* restore std font then return */
  return;
} /* doitbox */

/* **************  Work with symbol box which invokes a task *************** */
/*
  Standard redraw and invocation for a box displaying a symbol. Takes symbox indes
  character width of box and pixels for lower left corner.
*/
void dosymbox(box dobox,int asklen,long int* sav_font,long int* use_font,int* b_bottom,
	int* b_left,char* topic,char act){
/* where:  dobox is the box to do it in,
 *         asklen character width of the box,
 *         *topic which topic this button supports,
 *         *sav_font, *use_font font in current use, font to use within box,
 *         *b_bottom, *b_left pixel at lower left of box (supplied),
 *         act action to take (- is draw, ! is hilight and do  */

  int bottom, left;	/* pixel at lower left of box (supplied) */
/*   long int iupx,iupy;	position of << future >> popup */
  long int  s_font, u_font;	/* font in current use, font to use within box */
  long int elevchange,azichange,ifrlk;	/* current value to pass to fortran. */
  long int axt,ayt; /* centre for azim +- symbols  */
  long int sym,sz;      /* symbol and symbol size      */

  s_font = *sav_font; u_font = *use_font;
  bottom = *b_bottom; left = *b_left;
  if (s_font != u_font) winfnt_(&u_font);

  dobox.b_bottom = bottom - 2;  dobox.b_top = bottom - (f_height + 4);
  dobox.b_left = left;  dobox.b_right = dobox.b_left + (asklen * f_width);
  if (strncmp(topic, "aziplus", 7) == 0) {	/* assign relevant box structure */
    aziplus = dobox;          /* + arrow  */
  } else if (strncmp(topic, "aziminus", 8) == 0) {
    aziminus = dobox;          /* - arrow  */
  } else if (strncmp(topic, "elevplus", 8) == 0) {
    elevplus = dobox;          /* up arrow  */
  } else if (strncmp(topic, "elevminus", 9) == 0) {
    elevminus = dobox;          /* down arrow  */
  }
  if(act == '-') {
    xbox(dobox,fg,white,BMCLEAR |BMEDGES);   /* draw box with edges & text */
    axt = dobox.b_left+5;         /* points for symbol */
    ayt = dobox.b_bottom - (f_height/2);
    if (strncmp(topic, "aziplus", 7) == 0) {
      sym=30; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* + arrow  */
    } else if (strncmp(topic, "aziminus", 8) == 0) {
      sym=31; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* - arrow  */
    } else if (strncmp(topic, "elevplus", 8) == 0) {
      sym=1; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* up arrow  */
    } else if (strncmp(topic, "elevminus", 9) == 0) {
      sym=2; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* down arrow  */
    }
  } else if (act == '!') {
    xbox(dobox,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);        /* invert */
    XFlush(theDisp);
    XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
    if (s_font != u_font) winfnt_(&s_font);  /* restore std font during action */

    if (strncmp(topic, "aziplus", 7) == 0) {
      Timer(200);
      azichange = -10;	/* visual anitclockwise */
      ifrlk=0;
      chgazi_(&azichange,&ifrlk);  /* Deal with user selection of azimuth increment  */
    } else if (strncmp(topic, "aziminus", 8) == 0) {
      Timer(200);
      azichange = 10;	/* visual clockwise */
      ifrlk=0;
      chgazi_(&azichange,&ifrlk);  /* Deal with user selection of azimuth decrement  */
    } else if (strncmp(topic, "elevplus", 8) == 0) {
      Timer(200);
      elevchange = 5;
      ifrlk=0;
      chgelev_(&elevchange,&ifrlk);	/* call fortran with elevation increment */
    } else if (strncmp(topic, "elevminus", 9) == 0) {
      Timer(200);
      elevchange = -5;
      ifrlk=0;
      chgelev_(&elevchange,&ifrlk);	/* call fortran with elevation decrement */
    }

    if (s_font != u_font) winfnt_(&u_font);
    xbox(dobox,fg,white, BMCLEAR | BMEDGES);               /* clear box */
    axt = dobox.b_left+5;         /* points for symbol */
    ayt = dobox.b_bottom - (f_height/2);
    if (strncmp(topic, "aziplus", 7) == 0) {
      sym=30; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* + arrow  */
    } else if (strncmp(topic, "aziminus", 8) == 0) {
      sym=31; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* - arrow  */
    } else if (strncmp(topic, "elevplus", 8) == 0) {
      sym=1; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* up arrow  */
    } else if (strncmp(topic, "elevminus", 9) == 0) {
      sym=2; sz=0; esymbol_(&axt,&ayt,&sym,&sz);          /* down arrow  */
    }
  }
  XFlush(theDisp);  /* added to force draw */
  if (s_font != u_font) winfnt_(&s_font);  /* restore std font then return */
  return;
} /* dosymbox */

/* **************  Open an alt cmd box *************** */
/*
  Standard alternative command string box for dialog that needs one. Takes text to display
  character width of box, pixels for lower left corner.
*/
void altbox(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act){
/* where:  msg is the text in box,
 *         msglen,asklen character width of the text and input box,
 *         *b_bottom, *b_left pixel at lower left of box (supplied),
 *         act action to take (- is draw, ! is hilight and do  */

  XGlyphInfo info;
  XftDraw *draw;
  int lm1;		/* local string lengths found by test  */
  int  bottom, left;	/* pixel at lower left of box (supplied) */
  long int saved_font;
  int vfw;      /* pixels width for the box */
  int padding;  /* character difference between msglen and asklen */

  bottom = *b_bottom; left = *b_left;
  lm1=msglen;  padding=(asklen - msglen);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

// Use XftTextExtents8 to get the actual pixels needed for the string
// rather than assuming f_width is suitable.
  vfw=0;
  XftTextExtents8(theDisp,fst,msg,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff + (padding * f_width);  /* impose requested additional space */
//  fprintf(stderr,"altbox msg %s lm1 %d asklen %d vwf %d f_width %d asktimesfw %d\n",msg,lm1,asklen,vfw,f_width,(asklen * f_width));
  if( (asklen * f_width) > vfw ) vfw = asklen * f_width;
//  fprintf(stderr,"altbox new vfw is %d\n",vfw);

  altb.b_top = bottom - (f_height + 6);
  altb.b_bottom = bottom -2;
  altb.b_left = left;
  altb.b_right = altb.b_left + vfw;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  if(act == '-') {
    xbox(altb,fg,white,BMCLEAR |BMEDGES);   /* draw commands box with edges  */
    XftDrawString8(draw, &xft_color,fst,altb.b_left+3,altb.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if (act == '!') {
    xbox(altb,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);        /* invert box */
    XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
    XftDrawString8(draw, &xft_color,fst,altb.b_left+3,altb.b_bottom-3,(XftChar8 *) msg,lm1);
    XFlush(theDisp);
    XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
    Timer(200);
    xbox(altb,fg,white, BMCLEAR | BMEDGES);               /* clear box */
    XftDrawString8(draw, &xft_color,fst,altb.b_left+3,altb.b_bottom-3,(XftChar8 *) msg,lm1);
  }
  XFlush(theDisp);  /* added to force draw */
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* altbox */

/* **************  Open a 2nd alt cmd box *************** */
/*
  Standard alternative command string box for dialog that needs a second one.
  Takes text to display character width of box, pixels for lower left corner.
*/
void alt2box(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act){
/* where:  msg is the text in box,
 *         msglen,asklen character width of the text and input box,
 *         *b_bottom, *b_left pixel at lower left of box (supplied),
 *         act action to take (- is draw, ! is hilight and do  */

  XGlyphInfo info;
  XftDraw *draw;
  int lm1;		/* local string lengths found by test  */
  int  bottom, left;	/* pixel at lower left of box (supplied) */
  long int saved_font;
  int vfw;      /* pixels width for the box */
  int padding;  /* character difference between msglen and asklen */

  bottom = *b_bottom; left = *b_left;
  lm1=msglen;  padding=(asklen - msglen);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

// Use XftTextExtents8 to get the actual pixels needed for the string.
  vfw=0;
  XftTextExtents8(theDisp,fst,msg,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff + (padding * f_width);  /* impose requested additional space */
//  fprintf(stderr,"alt2box msg %s lm1 %d asklen %d vwf %d f_width %d asktimesfw %d\n",msg,lm1,asklen,vfw,f_width,(asklen * f_width));
  if( (asklen * f_width) > vfw ) vfw = asklen * f_width;
//  fprintf(stderr,"alt2box new vfw is %d\n",vfw);

  altc.b_top = bottom - (f_height + 6);
  altc.b_bottom = bottom -2;
  altc.b_left = left;
  altc.b_right = altc.b_left + vfw;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  if(act == '-') {
    xbox(altc,fg,white,BMCLEAR |BMEDGES);   /* draw commands box with edges  */
    XftDrawString8(draw, &xft_color,fst,altc.b_left+3,altc.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if (act == '!') {
    xbox(altc,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);        /* invert box */
    XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
    XftDrawString8(draw, &xft_color,fst,altc.b_left+3,altc.b_bottom-3,(XftChar8 *) msg,lm1);
    XFlush(theDisp);
    XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
    Timer(200);
    xbox(altc,fg,white, BMCLEAR | BMEDGES);               /* clear box */
    XftDrawString8(draw, &xft_color,fst,altc.b_left+3,altc.b_bottom-3,(XftChar8 *) msg,lm1);
  }
  XFlush(theDisp);  /* added to force draw */
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* alt2box */


/* **************  Open one of a b c d e f g box *************** */
/*
  Standard opening of a b c d boxes for dialogs that needs one or more
  of these. Takes text to display character width of box, pixels for
  lower left corner and a char indicator for which box to be working with.
  NOTE: expects boxes "a = b = c = d = e = f = g " to have been
  done in the calling function.
*/
void abcdboxs(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act){
/* where:  msg is the text in box,
 *         msglen,asklen character width of the text and input box,
 *         *b_bottom, *b_left pixel at lower left of box (supplied),
 *         act action to take (- is draw, ! is hilight and do  */

  XGlyphInfo info;
  XftDraw *draw;
  int lm1;		/* local string lengths  */
  int  bottom, left;	/* pixel at lower left of box (supplied) */
  long int saved_font;
  int vfw;      /* pixels width for the box */
  int padding;  /* character difference between msglen and asklen */

  bottom = *b_bottom; left = *b_left;
  lm1=msglen;  padding=(asklen - msglen);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

// Use XftTextExtents8 to get the actual pixels needed for the string.
  vfw=0;
  XftTextExtents8(theDisp,fst,msg,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff + (padding * f_width);  /* impose requested additional space */
//  fprintf(stderr,"abcdboxs msg %s lm1 %d asklen %d vwf %d f_width %d asktimesfw %d\n",msg,lm1,asklen,vfw,f_width,(asklen * f_width));
  if( (asklen * f_width) > vfw ) vfw = asklen * f_width;
//  fprintf(stderr,"abcdboxs new vfw is %d\n",vfw);

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  if( act == 'a') {
    a.b_top = bottom - (f_height + 6);
    a.b_bottom = bottom -2;
    a.b_left = left;
    a.b_right = a.b_left + vfw;
    xbox(a,fg,white,BMCLEAR |BMEDGES);   /* draw a box with edges  */
    XftDrawString8(draw, &xft_color,fst,a.b_left+3,a.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if ( act == 'b') {
    b.b_top = bottom - (f_height + 6);
    b.b_bottom = bottom -2;
    b.b_left = left;
    b.b_right = b.b_left + vfw;
    xbox(b,fg,white,BMCLEAR |BMEDGES);   /* draw b box with edges  */
    XftDrawString8(draw, &xft_color,fst,b.b_left+3,b.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if ( act == 'c') {
    c.b_top = bottom - (f_height + 6);
    c.b_bottom = bottom -2;
    c.b_left = left;
    c.b_right = c.b_left + vfw;
    xbox(c,fg,white,BMCLEAR |BMEDGES);   /* draw c box with edges  */
    XftDrawString8(draw, &xft_color,fst,c.b_left+3,c.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if ( act == 'd') {
    d.b_top = bottom - (f_height + 6);
    d.b_bottom = bottom -2;
    d.b_left = left;
    d.b_right = d.b_left + vfw;
    xbox(d,fg,white,BMCLEAR |BMEDGES);   /* draw d box with edges  */
    XftDrawString8(draw, &xft_color,fst,d.b_left+3,d.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if ( act == 'e') {
    e.b_top = bottom - (f_height + 6);
    e.b_bottom = bottom -2;
    e.b_left = left;
    e.b_right = e.b_left + vfw;
    xbox(e,fg,white,BMCLEAR |BMEDGES);   /* draw e box with edges  */
    XftDrawString8(draw, &xft_color,fst,e.b_left+3,e.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if ( act == 'f') {
    f.b_top = bottom - (f_height + 6);
    f.b_bottom = bottom -2;
    f.b_left = left;
    f.b_right = f.b_left + vfw;
    xbox(f,fg,white,BMCLEAR |BMEDGES);   /* draw f box with edges  */
    XftDrawString8(draw, &xft_color,fst,f.b_left+3,f.b_bottom-3,(XftChar8 *) msg,lm1);
  } else if ( act == 'g') {
    g.b_top = bottom - (f_height + 6);
    g.b_bottom = bottom -2;
    g.b_left = left;
    g.b_right = g.b_left + vfw +1;
    xbox(g,fg,white,BMCLEAR |BMEDGES);   /* draw g box with edges  */
    XftDrawString8(draw, &xft_color,fst,g.b_left+3,g.b_bottom-3,(XftChar8 *) msg,lm1);
  }
  XFlush(theDisp);  /* added to force draw */
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* abcdboxs */

/* **************  Open a dialogue input box *************** */
/*
 Passed the prompts which were sent to the dialogue box so that the
 correct position is defined. msgbx contains the prompts while
 askbx is reserved for user input.  If the width of the string
 would cause the input box to over-run the dialogue box then
 truncate the input box. If there is only a " " in the second
 prompt then position askbx against the left side.
*/
void openaskbox_(msg1,msg2,asklen,len1,len2)
  char      *msg1,*msg2; /* character strings for each line */
  int  len1,len2; /* lengths as supplied by fortran */
  long int  *asklen;     /* character width of the input box */
{
  XftDraw *draw;
  XGlyphInfo info;
  int lprompt,tprompt;   /* cursor position, prompt left side */
  int lm1, lm2;          /* local string lengths found by test  */
  int okbox_left, qbox_left, dbox_left;	/* positions of small boxes */
  long int saved_font;
  int vfw,vfwm1,vfwm2;

  ask_len = (int) *asklen;
  askmsg1 = msg1; askmsg2 = msg2; /* remember prompt character strings */
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=0;

  f_to_c_l(msg1,&len1,&lm1);
  XftTextExtents8(theDisp,fst,msg1,lm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskbox msg1 %s lm1 %d vwfm1 %d f_width %d timesfw %d\n",msg1,lm1,vfwm1,f_width,(lm1 * f_width));

  f_to_c_l(msg2,&len2,&lm2);
  XftTextExtents8(theDisp,fst,msg2,lm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskbox msg2 %s lm2 %d vwfm2 %d f_width %d timesfw %d\n",msg2,lm2,vfwm2,f_width,(lm2 * f_width));
  asklm1=lm1; asklm2=lm2; /* remember prompt lengths */

  askbx.b_top = msgbx.b_bottom - (f_height + 6);
  askbx.b_bottom = msgbx.b_bottom -2;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* ok box is 3rd box over and 7 char total to the right */
  okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
/* query box is 2rd box over and 4 char total to the right */
  qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
/* defaults box is 1 box over and 2 char total to the right */
  dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));

/* Set askbx against ok box and truncate editing box if necessary. */
  askbx.b_right = okbox_left - 5;
  askbx.b_left = askbx.b_right -((*asklen+2) * f_width);
  if (askbx.b_left < (msgbx.b_left + 5)) {
       askbx.b_left = msgbx.b_left + 5;
  }

/* determine left edge of prompt texts */
  lprompt = askbx.b_left - vfwm2;    /* lower prompt left */
  tprompt = askbx.b_left - vfwm1;    /* top prompt left */
  if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 
  asklprompt = lprompt;  /* remember both the top and lower */
  asktprompt = tprompt;

  msgbx.b_top =  xrt_height - ((f_height+4) * 2) - 3;
  msgbx.b_bottom=  xrt_height - 3;
  msgbx.b_left  = 2;
  msgbx.b_right = xrt_width - 2;

  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
    XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
    XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
  qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
  dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
  okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
  xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
  XFlush(theDisp);
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* openaskbox */

/* **************  Open a dialogue & command input box *************** */
/*
 Passed the prompts which were sent to the dialogue box so that the
 correct position is defined. msgbx contains the prompts, cmdbox shows
 alternative command (action) and askbx is reserved for user input.
 If the width of the string
 would cause the input box to over-run the dialogue box then
 truncate the input box. If there is only a " " in the second
 prompt then position askbx against the left side.
*/
void openaskaltbox_(msg1,msg2,alt,asklen,len1,len2,len3)
  char      *msg1,*msg2,*alt; /* character strings for each line and action message*/
  int  len1,len2,len3; /* lengths as supplied by fortran */
  long int  *asklen;     /* character width of the input box */
{
  XftDraw *draw;
  XGlyphInfo info;
  int lprompt,tprompt;   /* cursor position, prompt left side */
  int lm1, lm2, lm3;          /* local string lengths found by test  */
  int altbox_left, okbox_left, qbox_left, dbox_left;	/* positions of small boxes */
  long int saved_font;
  int vfw,vfwm1,vfwm2,vfwm3;

  ask_len = (int) *asklen;
  askmsg1 = msg1; askmsg2 = msg2; /* remember prompt character strings */
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=vfwm3=0;

  f_to_c_l(msg1,&len1,&lm1);
  XftTextExtents8(theDisp,fst,msg1,lm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskaltbox msg1 %s lm1 %d vwfm1 %d f_width %d timesfw %d\n",msg1,lm1,vfwm1,f_width,(lm1 * f_width));

  f_to_c_l(msg2,&len2,&lm2);
  XftTextExtents8(theDisp,fst,msg2,lm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskaltbox msg2 %s lm2 %d vwfm2 %d f_width %d timesfw %d\n",msg2,lm2,vfwm2,f_width,(lm2 * f_width));

  f_to_c_l(alt,&len3,&lm3);
  XftTextExtents8(theDisp,fst,alt,lm3,&info);
  if( info.xOff > vfwm3 ) vfwm3= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskaltbox alt %s lm3 %d vwfm3 %d f_width %d timesfw %d\n",alt,lm3,vfwm3,f_width,(lm3 * f_width));
  asklm1=lm1; asklm2=lm2; asklm3=lm3; /* remember prompt lengths */

  askbx.b_top = msgbx.b_bottom - (f_height + 6);
  askbx.b_bottom = msgbx.b_bottom -2;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* alt box is between ok and askbx  */
  altbox_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + lm3)));
/* ok box is 3rd box over and 7 char total to the right */
  okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
/* query box is 2rd box over and 4 char total to the right */
  qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
/* defaults box is 1 box over and 2 char total to the right */
  dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));

/* Set askbx against ok box and truncate editing box if necessary. */
  askbx.b_right = altbox_left - 5;
  askbx.b_left = askbx.b_right -((*asklen+2) * f_width);
  if (askbx.b_left < (msgbx.b_left + 5)) {
       askbx.b_left = msgbx.b_left + 5;
  }

/* determine left edge of prompt text */
  lprompt = askbx.b_left - vfwm2;    /* lower prompt left */
  tprompt = askbx.b_left - vfwm1;    /* top prompt left */
  if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 

  asklprompt = lprompt;   /* remember both the top and lower */
  asktprompt = tprompt;

  msgbx.b_top = xrt_height - ((f_height+4) * 2) - 3;
  msgbx.b_bottom= xrt_height - 3;
  msgbx.b_left  = 2;
  msgbx.b_right = xrt_width - 2;

  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
    XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
    XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);

  altbox(alt,lm3,lm3+1,&msgbx.b_bottom,&altbox_left,'-');
  qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
  dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
  okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
  xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  XFlush(theDisp);
  XftDrawDestroy(draw);
  return;
} /* openaskaltbox */

/* **************  Open a dialogue & cancel box *************** */
/*
 Passed the prompts which were sent to the dialogue box so that the
 correct position is defined. msgbx contains the prompts, cancelbox shows
 cancel command (action) and askbx is reserved for user input.
 If the width of the string
 would cause the input box to over-run the dialogue box then
 truncate the input box. If there is only a " " in the second
 prompt then position askbx against the left side.
*/
void openaskcnclbox_(msg1,msg2,cncl,asklen,len1,len2,len3)
  char      *msg1,*msg2,*cncl; /* character strings for each line and action message*/
  int  len1,len2,len3; /* lengths as supplied by fortran */
  long int  *asklen;     /* character width of the input box */
{
  XftDraw *draw;
  XGlyphInfo info;
  int lprompt,tprompt;   /* cursor position, prompt left side */
  int lm1, lm2, lm3;          /* local string lengths found by test  */
  int cnclbox_left, okbox_left, qbox_left, dbox_left;	/* positions of small boxes */
  long int saved_font;
  int vfw,vfwm1,vfwm2,vfwm3;

  ask_len = *asklen;
  askmsg1 = msg1; askmsg2 = msg2; /* remember prompt character strings */
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=vfwm3=0;

  f_to_c_l(msg1,&len1,&lm1);
  XftTextExtents8(theDisp,fst,msg1,lm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskcnclbox msg1 %s lm1 %d vwfm1 %d f_width %d timesfw %d\n",msg1,lm1,vfwm1,f_width,(lm1 * f_width));

  f_to_c_l(msg2,&len2,&lm2);
  XftTextExtents8(theDisp,fst,msg2,lm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskcnclbox msg2 %s lm2 %d vwfm2 %d f_width %d timesfw %d\n",msg2,lm2,vfwm2,f_width,(lm2 * f_width));

  f_to_c_l(cncl,&len3,&lm3);
  XftTextExtents8(theDisp,fst,cncl,lm3,&info);
  if( info.xOff > vfwm3 ) vfwm3= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskcnclbox cncl %s lm3 %d vwfm3 %d f_width %d timesfw %d\n",cncl,lm3,vfwm3,f_width,(lm3 * f_width));
  asklm1=lm1; asklm2=lm2; asklm3=lm3; /* remember prompt lengths */

  askbx.b_top = msgbx.b_bottom - (f_height + 6);
  askbx.b_bottom = msgbx.b_bottom -2;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* ok box is 4th box over and 8+lm3 char total to the right */
  okbox_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + lm3)));
/* query box is 3rd box over and 5+lm3 char total to the right */
  qbox_left = msgbx.b_right - ((3 * 5) + (f_width * (5 + lm3)));
/* defaults box is 2 box over and 3+lm3 char total to the right */
  dbox_left = msgbx.b_right - ((2 * 5) + (f_width * (3 + lm3)));
/* cncl box is on far right  */
  cnclbox_left = msgbx.b_right - ((1 * 5) + (f_width * (1 + lm3)));

/* Set askbx against ok box and truncate editing box if necessary. */
  askbx.b_right = okbox_left - 5;
  askbx.b_left = askbx.b_right -((*asklen+2) * f_width);
  if (askbx.b_left < (msgbx.b_left + 5)) {
       askbx.b_left = msgbx.b_left + 5;
  }

/* determine left edge of prompt text */
  lprompt = askbx.b_left - vfwm2;    /* lower prompt left */
  tprompt = askbx.b_left - vfwm1;    /* top prompt left */
  if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 
  asklprompt = lprompt;   /* remember the position */
  asktprompt = tprompt;

  msgbx.b_top = xrt_height - ((f_height+4) * 2) - 3;
  msgbx.b_bottom= xrt_height - 3;
  msgbx.b_left  = 2;
  msgbx.b_right = xrt_width - 2;

  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
    XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
    XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);

  altbox(cncl,lm3,lm3+1,&msgbx.b_bottom,&cnclbox_left,'-');
  qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
  dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
  okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
  xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  XFlush(theDisp);
  XftDrawDestroy(draw);
  return;
} /* openaskcnclbox */

/* **************  Open a dialogue & 2 command input boxs *************** */
/*
 Passed the prompts which were sent to the dialogue box so that the
 correct position is defined. msgbx contains the prompts, cmdbox &
 cmdbox2 shows alternative command (actions) and askbx is reserved for user input.
 If the width of the string
 would cause the input box to over-run the dialogue box then
 truncate the input box. If there is only a " " in the second
 prompt then position askbx against the left side.
*/
void openask2altbox_(msg1,msg2,alt,alt2,asklen,len1,len2,len3,len4)
  char      *msg1,*msg2,*alt,*alt2; /* character strings for each line and action messages*/
  int  len1,len2,len3,len4; /* lengths as supplied by fortran */
  long int  *asklen;     /* character width of the input box */
{
  XftDraw *draw;
  XGlyphInfo info;
  int lprompt,tprompt;   /* cursor position, prompt left side */
  int lm1, lm2, lm3, lm4;          /* local string lengths found by test  */
  int altbox_left, alt2box_left, okbox_left, qbox_left, dbox_left;	/* positions of small boxes */
  long int saved_font;
  int vfw,vfwm1,vfwm2,vfwm3,vfwm4;

  ask_len = (int) *asklen;
  askmsg1 = msg1; askmsg2 = msg2; /* remember prompt character strings */
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=vfwm3=vfwm4=0;

  f_to_c_l(msg1,&len1,&lm1);
  XftTextExtents8(theDisp,fst,msg1,lm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskalt2box msg1 %s lm1 %d vwfm1 %d f_width %d timesfw %d\n",msg1,lm1,vfwm1,f_width,(lm1 * f_width));

  f_to_c_l(msg2,&len2,&lm2);
  XftTextExtents8(theDisp,fst,msg2,lm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskalt2box msg2 %s lm2 %d vwfm2 %d f_width %d timesfw %d\n",msg2,lm2,vfwm2,f_width,(lm2 * f_width));

  f_to_c_l(alt,&len3,&lm3);
  XftTextExtents8(theDisp,fst,alt,lm3,&info);
  if( info.xOff > vfwm3 ) vfwm3= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskalt2box alt %s lm3 %d vwfm3 %d f_width %d timesfw %d\n",alt,lm3,vfwm3,f_width,(lm3 * f_width));

  f_to_c_l(alt2,&len4,&lm4);
  XftTextExtents8(theDisp,fst,alt2,lm4,&info);
  if( info.xOff > vfwm4 ) vfwm4= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"openaskalt2box alt2 %s lm4 %d vwfm4 %d f_width %d timesfw %d\n",alt2,lm4,vfwm4,f_width,(lm4 * f_width));

  asklm1=lm1; asklm2=lm2; asklm3=lm3; asklm4=lm4; /* remember prompt lengths */

  askbx.b_top = msgbx.b_bottom - (f_height + 6);
  askbx.b_bottom = msgbx.b_bottom -2;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* alt box is between 2nd alt and askbx  */
  altbox_left = msgbx.b_right - ((5 * 5) + (f_width * (9 + lm3 +lm4)));
/* 2nd alt box is between ok and 1st alt  */
  alt2box_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + lm4)));
/* ok box is 3rd box over and 7 char total to the right */
  okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
/* query box is 2rd box over and 4 char total to the right */
  qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
/* defaults box is 1 box over and 2 char total to the right */
  dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));

/* Set askbx against ok box and truncate editing box if necessary. */
  askbx.b_right = altbox_left - 5;
  askbx.b_left = askbx.b_right -((*asklen+2) * f_width);
  if (askbx.b_left < (msgbx.b_left + 5)) {
       askbx.b_left = msgbx.b_left + 5;
  }

/* determine left edge of prompt text */
  lprompt = askbx.b_left - vfwm2;    /* lower prompt left */
  tprompt = askbx.b_left - vfwm1;    /* top prompt left */
  if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 

  asklprompt = lprompt;   /* remember the position */
  asktprompt = tprompt;

  msgbx.b_top = xrt_height - ((f_height+4) * 2) - 3;
  msgbx.b_bottom= xrt_height - 3;
  msgbx.b_left  = 2;
  msgbx.b_right = xrt_width - 2;

  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
    XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
    XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);

  altbox(alt,lm3,lm3+1,&msgbx.b_bottom,&altbox_left,'-');
  alt2box(alt2,lm4,lm4+1,&msgbx.b_bottom,&alt2box_left,'-');
  qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
  dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
  okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
  xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  XFlush(theDisp);
  XftDrawDestroy(draw);
  return;
} /* openask2altbox */


/* **************  Redraw edited text and cursor ***** */
void update_edit_str(ebox,edstr,xbar,lstrlen)
  char *edstr;         /* string to display */
  box ebox;            /* box to update string within */
  int *xbar,*lstrlen;   /*  | position and character width */
{
  XftDraw *draw;
  int len = *lstrlen;
  int x1 = *xbar;
  xbox(ebox,fg,white,BMCLEAR |BMEDGES);   /* clear and re-draw ebox box and text */

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  XftDrawString8(draw, &xft_color,fst,ebox.b_left+f_width,ebox.b_bottom-3,(XftChar8 *) edstr,len);
  XftDrawDestroy(draw);
  XDrawLine(theDisp,win,theGC,x1-2,ebox.b_top+1,x1+2,ebox.b_top+1); /* mark with top & bottom T bars */
  XDrawLine(theDisp,win,theGC,x1-2,ebox.b_top+2,x1+2,ebox.b_top+2);
  XDrawLine(theDisp,win,theGC,x1,ebox.b_top+1,x1,ebox.b_top+4);
  XDrawLine(theDisp,win,theGC,x1-2,ebox.b_bottom-1,x1+2,ebox.b_bottom-1);
  XDrawLine(theDisp,win,theGC,x1-2,ebox.b_bottom-2,x1+2,ebox.b_bottom-2);
  XDrawLine(theDisp,win,theGC,x1,ebox.b_bottom-1,x1,ebox.b_bottom-4);
  XFlush(theDisp);
}

/* **************  track editing of text and cursor ***** */
void track_edit_str(sbuf,event,tx1,tf_len,tlen,tfitchars,toffsc,tno_valid_event)
  XEvent *event;
  char sbuf[124];      /* string to display */
  int *tx1,*tlen,*tf_len,*tfitchars;   /*  | position and avail character space and current width */
  int *toffsc,*tno_valid_event;
{
  KeySym     ks;
  static char buf[80];
  static int blen = 0;
  int x1 = *tx1;  /* chars between left of string & cursor */
  int len = *tlen;
  int f_len = *tf_len;
  int fitchars = *tfitchars;
  int offsc = *toffsc;
  int no_valid_event = *tno_valid_event;
  int i,lstrlen;

/* Because event is passed as a pointer */
  blen = XLookupString((XKeyEvent *)event,buf,80,&ks,(XComposeStatus *) NULL);
  if (ks==XK_Left || ks==XK_KP_Left) {  /* left arrow pressed */
    if((x1-f_width) > askbx.b_left) {
      x1 = x1 - f_width; offsc = offsc - 1;
      if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
      update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
    } else {
     XBell(theDisp, 50);  /*  buf[0] is left arrow but at left edge  */
    }
  } else if (ks==XK_Right || ks==XK_KP_Right) {  /* right arrow pressed */
    if((x1+f_width) < askbx.b_right) {
      x1 = x1 + f_width; offsc = offsc + 1;
      if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
      update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
    } else {
     XBell(theDisp, 50);  /* buf[0] is right arrow but at right edge */
    }
  }
  if(blen > 0) {
    if (buf[0] == '\r' || buf[0] == '\n' ) {
      okbox("ok",2,3,&msgbx.b_bottom,&okb.b_left,'!'); /* brief hilight after return or newline */
      no_valid_event = FALSE;
    } else if (buf[0] == '\177' || buf[0] == '\010') {  /*  buf[0] is DEL or BS  */
      if((x1-f_width) > askbx.b_left) {
/* debug fprintf(stderr,"track_edit_str *** sbuf %s char @ %c offsc %d len %d %d\n",sbuf,sbuf[offsc],offsc,len,f_len);  */
        for (i=offsc; i<f_len; i++) sbuf[i-1] = sbuf[i];  /* shift characters above offsc << */
        if(offsc <= len) {
          sbuf[len]=' ';  /* last nonblank char becomes a space if | within string */
          len = len - 1;  /* decrement len */
        }
        x1 = x1 - f_width; offsc = offsc - 1;  /* shift pointers */
/* debug fprintf(stderr,"track_edit_str now sbuf %s char @ %c offsc %d len %d %d\n",sbuf,sbuf[offsc],offsc,len,f_len);  */
        if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
        update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
      } else {
        XBell(theDisp, 50);  /* buf[0] is DEL but at left edge  */
      }
    } else if (buf[0] == '\011') {
      XBell(theDisp, 50);
/* debug fprintf(stderr,"editing: TAB character ignored\n"); */
    } else if (buf[0] == '\033') {
      XBell(theDisp, 50);
/* debug fprintf(stderr,"editing: ESC character ignored. \n"); */
    } else if (buf[0] >= ' ' && buf[0] < '\177') {
/* debug fprintf(stderr,"track_edit_str buf[0] is insert %c \n",buf[0]);   */
      if ((len+1) <= f_len) {
        for (i=f_len; i>offsc; i--) sbuf[i] = sbuf[i-1];  /* shift characters above offsc >> */
        sbuf[offsc] = buf[0];  /* insert the new character */
        x1 = x1 + f_width; offsc = offsc + 1; len = len + 1;
/* debug fprintf(stderr,"track_edit_str now sbuf %s char @ %c %d %d \n",sbuf,sbuf[offsc],offsc,len);  */
        if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
        update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
      } else {
        XBell(theDisp, 50);
        fprintf(stderr,"editing: char string is full.\n");
      }
    }
    if(blen >1) fprintf(stderr,"track_edit_str 2nd buf char %c \n",buf[1]);
    if(blen >2) fprintf(stderr,"track_edit_str 3rd buf char %c \n",buf[2]);
  } else {
/* debug fprintf(stderr,"track_edit_str nothing in buf \n");  */
  }
  *tx1 = x1;  /* pass back updated values */
  *tlen = len;
  *toffsc = offsc;
  *tno_valid_event = no_valid_event;
  return;
}


/* *************** ESRU help message update. *************** */
/*
 This function takes an array of strings from f77 and stores
 it in the static array help_list for subsequent use by
 other functions.
*/
void updhelp_(items,nitmsptr,iw,len_items)
  char      *items;         /* f77 array of help text strings    */
  long int  *nitmsptr;      /* number of help lines to display   */
  long int  *iw;            /* actual max char width in items    */
  int  len_items;           /* length of help string from f77    */
{
  int	i,j,k;
  int	nitms = (int) *nitmsptr;
  char 	*item_local = items;	/* working copy of intput string */

  help_width = (int) *iw;	/* remember width of current help text */
  help_lines = nitms;	/* remember number of help lines */
  if(help_lines == 0)return;	/* don't bother if no help */

/* use similar logic to updmenu */
  k = 0;
  for(i = 0; i < nitms; i++) {	/* for each line...  */
    for(j = 0; j < 72; j++) {	/* for each character...  */
      help_list[i][j] = item_local[k];
      k = k +1;   /* increment for next char in items (a fortran string array does not have
                     nulls between strings in array, it just looks like one long string) */
    }
    help_list[i][71] = '\0';	/* write terminator  */
  }
  return;
}

/* *************** ESRU pop-up help box. *************** */
/*
 This function takes current help text and displays it in a box
 and waits for the user to press a key or click a mouse to get rid of it.
 If a long list paging is supported, if additional text is available
 then an extra "show me more" box is included.
 If the longest string will not fit in the width of the box then a
 smaller font will be used.
*/
void egphelp_(impx,impy,ipflg,ishowmoreflg,uresp)
  long int  *impx,*impy;	/* box position (if 0,0 use default) */
  long int  *ipflg,*ishowmoreflg;	/* paging if ipflg=1, showmore box if ishowmoreflg=1 */
  long int  *uresp;		/* user response, normally 0 to end, 1 is showmore ok */
{
/* Local variables   */
  XEvent event;
  XWindowAttributes wa;
  Pixmap under;		        /* to save image under help box  */
  XftDraw *draw;
  XGlyphInfo info;
  long int saved_font, use_font, changed_font;
  int   h_height, h_width, xb,yb,x,y,i,iy,pflg,showmoreflg;
  int   u_height, u_width;      /* size of the under pixmap */
  int	mifull,ist,ilen,ipm,m;
  float	page;
  long int ext,eyt,exb,eyb; /* centre for up/down symbols  */
  long int sym,sz;      /* symbol and symbol size      */
  box	helpbx, ghelpbx, dismissbx;	/* help display, outer box, dismiss box */
  box	prevbx, nextbx, showmorebx;	/* previous, next and showmore boxs */
  Bool exp = 1;
  int	no_valid_event,config_altered;
  KeySym     ks;
  static char buf[80];
  static int blen = 0;
  unsigned int start_height,start_width;
  int vfw;
  long int help_font = 5;  /* one up from smallest proportional font */

  ilen = help_lines;
  if(ilen == 0)return;	/* don't bother if no help */

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width; config_altered = 0;

  xb = (int) *impx;  yb = (int) *impy; pflg = (int) *ipflg; showmoreflg = (int) *ishowmoreflg;
  changed_font = 0;

/* Help messages can always be in proportional font. Save existing and reset if necessary */
  saved_font = use_font = current_font;
  if ( use_font != help_font ) { winfnt_(&help_font); use_font = help_font; changed_font = 1; }
  
  h_height = (20*(f_height+1))+20;  /* include a bit of extra space  */
  if(ilen <= 20) h_height = (ilen*(f_height+1))+20;  /* include a bit of extra space  */
  h_width = (help_width*f_width)+25;	  /* box slightly wider than longest line */

/* Use XftTextExtents8 to loop through the help strings and
   detemine the maximum pixel width needed.
 */
  vfw=0;
  for ( i = 0; i < ilen; i++ ) {
     if ( (int) strlen(help_list[i]) > 0 ) {
       XftTextExtents8(theDisp,fst,help_list[i],help_width,&info);
       if( info.xOff > vfw ) vfw= info.xOff;
     }
  }
  // fprintf(stderr,"help is h_width %d or vfw %d pixels wide\n",h_width,vfw);
  if ( vfw+10 > h_width ) h_width=vfw+10;
/*
 If a specific box starting point has been passed then try to honor
 this otherwise place it slightly above the dialogue box.
*/
  if (xb == 0) {
    xb= msgbx.b_left+70;  yb = msgbx.b_top-60;
  }

/* Create the help box but keep it from going above the display. */
  if (yb - h_height < 50) {
    helpbx.b_top   = yb - h_height + 50;  helpbx.b_bottom= yb + 50;
  } else {
    helpbx.b_top   = yb - h_height;  helpbx.b_bottom= yb;
  }
  if (use_font != butn_fnt) winfnt_(&butn_fnt);
  helpbx.b_left  = xb;  helpbx.b_right = xb + h_width;
  ghelpbx.b_top   = helpbx.b_top - 5;	/* border with space for buttons */
  ghelpbx.b_bottom= helpbx.b_bottom + f_height +8;
  ghelpbx.b_left  = helpbx.b_left - 5;  ghelpbx.b_right = helpbx.b_right + 5;

  u_width = WIDTH(ghelpbx); u_height = HEIGHT(ghelpbx); /* width & height area to save */
  under = XCreatePixmap(theDisp,win,(unsigned int) u_width,(unsigned int) u_height,dispDEEP);
  box_to_pix(win,ghelpbx,under,u_width,u_height);   /* save rect under ghelpbx to under */

  dismissbx.b_left = ghelpbx.b_left + 8;
  dismissbx.b_right = dismissbx.b_left + (9 * f_width);
  dismissbx.b_bottom= helpbx.b_bottom + f_height + 5;
  dismissbx.b_top = helpbx.b_bottom + 3;
  if( pflg == 1) {
    prevbx.b_left = ghelpbx.b_right - ( 24 * f_width);
    prevbx.b_right = prevbx.b_left + 20;
    prevbx.b_bottom= dismissbx.b_bottom; prevbx.b_top = dismissbx.b_top;
    ext = prevbx.b_left+8;         /* points for prev arrow */
    eyt = prevbx.b_bottom - (f_height/2);
    nextbx.b_left = ghelpbx.b_right - ( 20 * f_width);
    nextbx.b_right = nextbx.b_left + 20;
    nextbx.b_bottom= dismissbx.b_bottom; nextbx.b_top = dismissbx.b_top;
    eyb = nextbx.b_bottom - (f_height/2);
    exb = nextbx.b_left+8;        /* points for next arrow */
  }
  if( showmoreflg == 1) {
    showmorebx.b_left = ghelpbx.b_right - ( 7 * f_width);
    showmorebx.b_right = showmorebx.b_left + (5 * f_width);
    showmorebx.b_bottom= dismissbx.b_bottom; showmorebx.b_top = dismissbx.b_top;
  }

  xbox(ghelpbx,fg,gfeedfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
  xbox(ghelpbx,fg,bg,BMEDGES);
  xbox(helpbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */

  xbox(dismissbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  if (use_font != butn_fnt) winfnt_(&butn_fnt);
    XftDrawString8(draw, &xft_color,fst,dismissbx.b_left+3,dismissbx.b_bottom-2,(XftChar8 *) "dismiss",7);
  if( showmoreflg == 1) {
    xbox(showmorebx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
    XftDrawString8(draw, &xft_color,fst,showmorebx.b_left+3,showmorebx.b_bottom-2,(XftChar8 *) "more",4);
  }
  winfnt_(&use_font);

/* display lines of text.  */
  mifull = 20;
  ist = 0;
  if(pflg == 1) {
    page = (float) (ist+mifull) / (float) mifull;
    if (page < 1.0) page = 1.0;
    ipm = (int) page;
  }
  if(ilen <= 20) {
    for ( i = 0; i < ilen; i++ ) {
      iy = helpbx.b_top + ((i + 1) * (f_height+1)) + 10;
      if ( (int) strlen(help_list[i]) == 0 ) {
          XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
      } else if ( (int) strlen(help_list[i]) > 0 ){
          XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
      }
    }
  } else {
    for ( i = 0; i < 20; i++ ) {
      iy = helpbx.b_top + ((i + 1) * (f_height+1)) + 10;
      if ( (int) strlen(help_list[i]) == 0 ) {
          XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
      } else if ( (int) strlen(help_list[i]) > 0 ){
        XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
      }
    }
    if( pflg == 1) {
      xbox(prevbx,fg,white,BMCLEAR|BMEDGES); sym=1; sz=0; esymbol_(&ext,&eyt,&sym,&sz);	/* prev arrow  */
      xbox(nextbx,fg,white,BMCLEAR|BMEDGES); sym=2; sz=0; esymbol_(&exb,&eyb,&sym,&sz);	/* next arrow  */
    }
  }
  no_valid_event = TRUE;
  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"egphelp: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(ghelpbx,fg,gfeedfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
          xbox(ghelpbx,fg,bg,BMEDGES);
          xbox(helpbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
          xbox(dismissbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
          if (use_font != butn_fnt) winfnt_(&butn_fnt);
          XftDrawString8(draw, &xft_color,fst,dismissbx.b_left+3,dismissbx.b_bottom-2,(XftChar8 *) "dismiss",7);
          if( showmoreflg == 1) {
            xbox(showmorebx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
            XftDrawString8(draw, &xft_color,fst,showmorebx.b_left+3,showmorebx.b_bottom-2,(XftChar8 *) "more",4);
          }
          winfnt_(&use_font);
          if(ilen <= 20) {
            for ( i = 0; i < ilen; i++ ) {
              iy = helpbx.b_top + ((i + 1) * (f_height+1)) + 10;
              if ( (int) strlen(help_list[i]) == 0 ) {
                  XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
             } else if ( (int) strlen(help_list[i]) > 0 ){
                XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
              }
            }
          } else {
            for ( i = 0; i < 20; i++ ) {
              iy = helpbx.b_top + ((i + 1) * (f_height+1)) + 10;
              if ( (int) strlen(help_list[i]) == 0 ) {
                  XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
              } else if ( (int) strlen(help_list[i]) > 0 ){
                XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
              }
            }
            if( pflg == 1) {
              xbox(prevbx,fg,white,BMCLEAR|BMEDGES); sym=1; sz=0; esymbol_(&ext,&eyt,&sym,&sz);	/* prev arrow  */
              xbox(nextbx,fg,white,BMCLEAR|BMEDGES); sym=2; sz=0; esymbol_(&exb,&eyb,&sym,&sz);	/* next arrow  */
            }
          }
          XFlush(theDisp);
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == wa.height && start_width == wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"egphelp detected configure event\n"); */
          config_altered = 1;
          refreshenv_();
          xbox(ghelpbx,fg,gfeedfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
          xbox(ghelpbx,fg,bg,BMEDGES);
          xbox(helpbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
          xbox(dismissbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
          if (use_font != butn_fnt) winfnt_(&butn_fnt);
          XftDrawString8(draw, &xft_color,fst,dismissbx.b_left+3,dismissbx.b_bottom-2,(XftChar8 *) "dismiss",7);
          if( showmoreflg == 1) {
            xbox(showmorebx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
            XftDrawString8(draw, &xft_color,fst,showmorebx.b_left+3,showmorebx.b_bottom-2,(XftChar8 *) "show more",9); 
          }
          winfnt_(&use_font);
          if(ilen <= 20) {
            for ( i = 0; i < ilen; i++ ) {
              iy = helpbx.b_top + ((i + 1) * (f_height+1)) + 10;
              if ( (int) strlen(help_list[i]) == 0 ) {
                XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
              } else if ( (int) strlen(help_list[i]) > 0 ){
                XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
              }
            }
          } else {
            for ( i = 0; i < 20; i++ ) {
              iy = helpbx.b_top + ((i + 1) * (f_height+1)) + 10;
              if ( (int) strlen(help_list[i]) == 0 ) {
                XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
              } else if ( (int) strlen(help_list[i]) > 0 ){
                XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
              }
            }
            if( pflg == 1) {
              xbox(prevbx,fg,white,BMCLEAR|BMEDGES); sym=1; sz=0; esymbol_(&ext,&eyt,&sym,&sz);	/* prev arrow  */
              xbox(nextbx,fg,white,BMCLEAR|BMEDGES); sym=2; sz=0; esymbol_(&exb,&eyb,&sym,&sz);	/* next arrow  */
            }
          }
          XFlush(theDisp);
        }
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        if ( pflg == 1 && xboxinside(prevbx,x,y)){
           saved_font = current_font;
           if (saved_font != use_font) winfnt_(&use_font);
           xbox(helpbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
           xbox(prevbx,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
           Timer(100);

 /* page back and redraw hellp text */
           if(ipm > 1) ist = ist - mifull;
           page = (float) (ist+mifull) / (float) mifull;
           if (page < 1.0) page = 1.0;
           ipm = (int) page;
           m = 0;
           for ( i = 0; i < ilen; i++ ) {
             if( i >= ist && i < (ist+mifull)) {
               m = m + 1;
               iy = helpbx.b_top + (m * (f_height+1)) + 10;
               if ( (int) strlen(help_list[i]) == 0 ) {
                 XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
               } else if ( (int) strlen(help_list[i]) > 0 ){
                 XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
               }
             }
           }
           Timer(100);
           xbox(prevbx,fg,white, BMCLEAR | BMEDGES);
           sym=2; sz=0; esymbol_(&exb,&eyb,&sym,&sz); /* prev arrow  */
   	   break;
         } else if ( pflg == 1 && xboxinside(nextbx,x,y)){
           saved_font = current_font;
           if (saved_font != use_font) winfnt_(&use_font);
           xbox(helpbx,fg,white,BMCLEAR|BMEDGES);	/* clear help display box  */
           xbox(nextbx,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
           Timer(100);

 /* page forward and redraw help text */
           if((ist+mifull) < ilen) ist = ist + mifull;
           page = (float) (ist+mifull) / (float) mifull;
           if (page < 1.0) page = 1.0;
           ipm = (int) page;
           m = 0;
           for ( i = 0; i < ilen; i++ ) {
             if( i >= ist && i < (ist+mifull)) {
               m = m + 1;
               iy = helpbx.b_top + (m * (f_height+1)) + 10;
               if ( (int) strlen(help_list[i]) == 0 ) {
                 XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
               } else if ( (int) strlen(help_list[i]) > 0 ){
                 XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
               }
             }
           }
           Timer(100);
           xbox(nextbx,fg,white, BMCLEAR | BMEDGES);
           sym=1; sz=0; esymbol_(&ext,&eyt,&sym,&sz); /* next arrow  */
   	   break;
         } else if (xboxinside(dismissbx,x,y)){
           no_valid_event = FALSE;
           *uresp = 0;
           xbox(helpbx,fg,white,BMEDGES|BMCLEAR);	/* clear help box */
           break;
         } else if (showmoreflg == 1 && xboxinside(showmorebx,x,y)){
           no_valid_event = FALSE;
           *uresp = 1;
           break;
         }
      case KeyPress:	/* (XKeyEvent)&ev */
        blen = XLookupString((XKeyEvent*)&event,buf,80,&ks,(XComposeStatus *) NULL);
        if(blen > 0) {
          if (buf[0] == '\r' || buf[0] == '\n' || buf[0] == ' ' ) {
            no_valid_event = FALSE;
            *uresp = 0;
   	    break;
          } else if (buf[0] == '>' && pflg == 1 ) {  /*  > key so move to next page  */
            saved_font = current_font;
            if (saved_font != use_font) winfnt_(&use_font);
            xbox(helpbx,fg,white,BMCLEAR|BMEDGES);	/* clear help display box  */
            xbox(nextbx,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
            Timer(100);
            if((ist+mifull) < ilen) ist = ist + mifull;
            page = (float) (ist+mifull) / (float) mifull;
            if (page < 1.0) page = 1.0;
            ipm = (int) page;
            m = 0;
            for ( i = 0; i < ilen; i++ ) {
              if( i >= ist && i < (ist+mifull)) {
                m = m + 1;
                iy = helpbx.b_top + (m * (f_height+1)) + 10;
                if ( (int) strlen(help_list[i]) == 0 ) {
                  XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
                } else if ( (int) strlen(help_list[i]) > 0 ){
                  XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
                }
              }
            }
            Timer(100);
            xbox(nextbx,fg,white, BMCLEAR | BMEDGES);
            sym=1; sz=0; esymbol_(&ext,&eyt,&sym,&sz); /* next arrow  */
            break;
          } else if (buf[0] == '<' && pflg == 1 ) {  /*  > key so move to previous page  */
            saved_font = current_font;
            if (saved_font != use_font) winfnt_(&use_font);
            xbox(helpbx,fg,white,BMCLEAR|BMEDGES);	/* draw help display box  */
            xbox(prevbx,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
            Timer(100);
            if(ipm > 1) ist = ist - mifull;
            page = (float) (ist+mifull) / (float) mifull;
            if (page < 1.0) page = 1.0;
            ipm = (int) page;
            m = 0;
            for ( i = 0; i < ilen; i++ ) {
              if( i >= ist && i < (ist+mifull)) {
                m = m + 1;
                iy = helpbx.b_top + (m * (f_height+1)) + 10;
                if ( (int) strlen(help_list[i]) == 0 ) {
                  XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) "  ",2);
                } else if ( (int) strlen(help_list[i]) > 0 ){
                  XftDrawString8(draw, &xft_color,fst,helpbx.b_left+10,iy,(XftChar8 *) help_list[i],help_width);
                }
              }
            }
            Timer(100);
            xbox(prevbx,fg,white, BMCLEAR | BMEDGES);
            sym=2; sz=0; esymbol_(&exb,&eyb,&sym,&sz); /* prev arrow  */
   	    break;
          }
        }
    }
  }

  XClearArea(theDisp,win,ghelpbx.b_left,ghelpbx.b_top,(unsigned int) u_width,(unsigned int) u_height,exp);
  if(config_altered == 0)pix_to_box(under,u_width,u_height,ghelpbx,win);
  XFreePixmap(theDisp, under);
  XftDrawDestroy(draw);
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  if (changed_font == 1) winfnt_(&saved_font);  /* Restore original font.  */
  XFlush(theDisp); /* added to force draw */
  if(config_altered == 1) refreshenv_();
}

/* **************  Get string from dialogue input box *************** */
/*
 Code implements control of a single line input facility.
 Input begins when cursor is within askbx.
*/
void askdialog_(sstr,id,iq,f_len)
  char *sstr;         /* string returned to fortran (remember strip end mark) */
  long int *id,*iq;   /* flag for default and help querry */
  int  f_len;   /* character width of the string from fortran */
{
  XEvent event;
  XWindowAttributes wa;
  XGlyphInfo info;
  XftDraw *draw;
  static char sbuf[144];
  int b_width;   /* b_width  pixels w/in box */
  int fitchars,offsc,x1,x2,fitpix;  /* chars able to fit within box, chars between left of string & cursor */
  int	no_valid_event = TRUE;
  int	initial_button_in = FALSE;
  int x,y,len,lstrlen;
  int okbox_left, qbox_left, dbox_left;	/* positions of small boxes */
  long int impx,impy,ipflg,ishowmoreflg,uresp;
  long int saved_font;
  int initial_f_height;  /* font height used for the small boxes */
  unsigned int start_height,start_width;
  int lprompt,tprompt;   /* cursor position, prompt left side */
  int iaux;         /* unused return from aux_menu      */
  int vfw,vfwm1,vfwm2;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=0;

  XftTextExtents8(theDisp,fst,askmsg1,asklm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"askdialog askmsg1 %s asklm1 %d vwfm1 %d f_width %d timesfw %d\n",askmsg1,asklm1,vfwm1,f_width,(asklm1 * f_width));

  XftTextExtents8(theDisp,fst,askmsg2,asklm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"askdialog askmsg2 %s asklm2 %d vwfm2 %d f_width %d timesfw %d\n",askmsg2,asklm2,vfwm2,f_width,(asklm2 * f_width));

/* ok box is 3rd box over and 7 char total to the right */
  okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
/* query box is 2rd box over and 4 char total to the right */
  qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
/* defaults box is 1 box over and 2 char total to the right */
  dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));
  initial_f_height = f_height;  /* remember in case of a resize */

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/*
 Find actual string length and truncate when printing to fit within box.
*/
  strncpy(sbuf,"                                                                                                ",96);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);
  f_to_c_l(sstr,&f_len,&len); strncpy(sbuf,sstr,(unsigned int)len);

  b_width = WIDTH(askbx);
  fitchars = ((b_width - f_width) / f_width);
  XftTextExtents8(theDisp,fst,sbuf,len,&info);
  fitpix = info.width;
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
  XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
  XFlush(theDisp);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,cross_cursor);

  /* Place cursor at the end of the existing string */
  offsc = len;
  x1 = (askbx.b_left+f_width) + (offsc * f_width);
  if( lstrlen == 1 ) {
    offsc = 1;
    x1 = (askbx.b_left+f_width) + (offsc * f_width);
  }
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; } 
  update_edit_str(askbx,sbuf,&x1,&lstrlen);   /* clear and re-draw askbx box and text */
  initial_button_in = TRUE; 

  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"askdialog: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (initial_f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"askdialog detected configure event\n"); */
          refreshenv_();
	  /* Window resized so force update of the positions of the various boxes
             including the position of askbx. */
          okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
          qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
          dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));

          /* Figure out where askbx is now. */
          askbx.b_right = okbox_left - 5;
          askbx.b_left = askbx.b_right -((ask_len+2) * f_width);
          askbx.b_top = msgbx.b_bottom - (initial_f_height + 6);  /* use initial font height */
          askbx.b_bottom = msgbx.b_bottom -2;
          if (askbx.b_left < (msgbx.b_left + 5)) {
            askbx.b_left = msgbx.b_left + 5;
          }

          /* Determine new left edge of prompt text. */
          lprompt = askbx.b_left - vfwm2;    /* lower prompt left */
          tprompt = askbx.b_left - vfwm1;    /* top prompt left */
          if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 
          asklprompt = lprompt;   /* remember the position */
          asktprompt = tprompt;

          /* Redraw the prompt strings and then the boxes. One
             remaining glitch - the font seems to be smaller? */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (initial_f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        if (xboxinside(askbx,x,y)){
          offsc = ((x - (askbx.b_left+f_width)) / f_width);
          x1 = (askbx.b_left+f_width) + (offsc * f_width);
          x2 = (askbx.b_left+f_width) + (lstrlen * f_width);
/* debug  fprintf(stderr,"textfollow a: x %d y %d x1 %d x2 %d offsc %d %c\n",x,y,x1,x2,offsc,sbuf[offsc]); */
          if( lstrlen == 1 ) {
            offsc = 1;
            x1 = (askbx.b_left+f_width) + (offsc * f_width);
          }
          if (x2 < x1 ) {
            offsc = lstrlen;
            x1 = (askbx.b_left+f_width) + (offsc * f_width);
          }
/* debug fprintf(stderr,"textfollow b: fitpix %d x %d y %d x1 %d offsc %d %c\n",fitpix,x,y,x1,offsc,sbuf[offsc]); */
          if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
          update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
/* if at this point sbuf is blank then reset x1 */
          initial_button_in = TRUE;
          break;
        } else if (xboxinside(okb,x,y)){
          okbox("ok",2,3,&msgbx.b_bottom,&okb.b_left,'!'); /* brief hilight */
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(defb,x,y)){
          dbox("d",1,2,&msgbx.b_bottom,&defb.b_left,'!'); /* brief hilight */
          *id = 1;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(querb,x,y)){
          qbox_("?",1,2,&msgbx.b_bottom,&querb.b_left,'!'); /* brief hilight */
	  impx = 0; impy = 0; ishowmoreflg = 0; uresp = 0;
          if (help_lines <= 20) ipflg = 0;
          if (help_lines > 20) ipflg = 1;
          egphelp_(&impx,&impy,&ipflg,&ishowmoreflg,&uresp);
          break;
        } else {
          iaux = aux_menu((XEvent *) &event);	/* check and see if text scrolled etc. */
          /* redraw the dialog */ 
/* debug  fprintf(stderr,"Inside askdialog display x %d y %d\n",x,y);  */
          if (saved_font != current_font) winfnt_(&saved_font);
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
          no_valid_event = TRUE;
          break;
        }
      case KeyPress:	/* if initial_button_in set then process keystroke */
        if (initial_button_in) {
          track_edit_str(sbuf,&event,&x1,&f_len,&len,&fitchars,&offsc,&no_valid_event);
         }
        break;
    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  strncpy(sstr,sbuf,(unsigned int)f_len);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor);
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  return;
} /* askdialog */

/* **************  Get string from dialogue & alt command input box *************** */
/*
 Code implements control of a single line input facility with alt control detect.
 Input begins when cursor is within askbx.
*/
void askaltdialog_(sstr,alt,id,iq,f_len,a_len)
  char *sstr,*alt;         /* string returned to fortran (remember strip end mark) */
  long int *id,*iq;   /* flag for default and help querry */
  int  f_len,a_len;   /* character width of the string from fortran */
{
  XEvent event;
  XWindowAttributes wa;
  XGlyphInfo info;
  XftDraw *draw;
  static char sbuf[144];
  int b_width;   /* b_width  pixels w/in box */
  int fitchars,offsc,x1,fitpix;  /* chars able to fit within box, chars between left of string & cursor */
  int	no_valid_event = TRUE;
  int	initial_button_in = FALSE;
  int x,y,len,lstrlen,lm3,iaux;
  long int impx,impy,ipflg,ishowmoreflg,uresp;
  long int saved_font;
  int okbox_left, qbox_left, dbox_left, altbox_left;	/* positions of small boxes */
  int initial_f_height;  /* font height used for the small boxes */
  unsigned int start_height,start_width;
  int lprompt,tprompt;   /* cursor position, prompt left side */
  int vfw,vfwm1,vfwm2;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=0;

  XftTextExtents8(theDisp,fst,askmsg1,asklm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"askdialog askmsg1 %s asklm1 %d vwfm1 %d f_width %d timesfw %d\n",askmsg1,asklm1,vfwm1,f_width,(asklm1 * f_width));

  XftTextExtents8(theDisp,fst,askmsg2,asklm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"askdialog askmsg2 %s asklm2 %d vwfm2 %d f_width %d timesfw %d\n",askmsg2,asklm2,vfwm2,f_width,(asklm2 * f_width));

/* alt box is between ok and askbx  */
  altbox_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + asklm3)));
/* ok box is 3rd box over and 7 char total to the right */
  okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
/* query box is 2rd box over and 4 char total to the right */
  qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
/* defaults box is 1 box over and 2 char total to the right */
  dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));
  initial_f_height = f_height;  /* remember in case of a resize */

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
/*
 Find actual string length and truncate when printing to fit within box.
*/
  strncpy(sbuf,"                                                                                                ",96);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);
  f_to_c_l(sstr,&f_len,&len); strncpy(sbuf,sstr,(unsigned int)len);
  f_to_c_l(alt,&a_len,&lm3);

  b_width = WIDTH(askbx);
  fitchars = ((b_width - f_width) / f_width);
  XftTextExtents8(theDisp,fst,sbuf,len,&info);
  fitpix = info.width;
/* debug  fprintf(stderr,"textfollow: sbuf is %s %d %d %d %d\n",sbuf,f_len,len,fitchars,fitpix); */
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
  XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
  XFlush(theDisp);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,cross_cursor);

/* Place cursor at the end of the existing string */
  offsc = len;
  x1 = (askbx.b_left+f_width) + (offsc * f_width);
  if( lstrlen == 1 ) {
    offsc = 1;
    x1 = (askbx.b_left+f_width) + (offsc * f_width);
  }
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; } 
  update_edit_str(askbx,sbuf,&x1,&lstrlen);   /* clear and re-draw askbx box and text */
  initial_button_in = TRUE; 

  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"askaltdialog: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (initial_f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          altbox(alt,asklm3,asklm3+1,&msgbx.b_bottom,&altbox_left,'-');
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"askaltdialog detected configure event\n"); */
          refreshenv_();
	  /* Window resized so force update of the positions of the various boxes
             including the position of askbx. */
          altbox_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + asklm3)));
          okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
          qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
          dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));

          /* Figure out where askbx is now. */
          askbx.b_right = altbox_left - 5;
          askbx.b_left = askbx.b_right -((ask_len+2) * f_width);
          askbx.b_top = msgbx.b_bottom - (initial_f_height + 6);  /* use initial font height */
          askbx.b_bottom = msgbx.b_bottom -2;
          if (askbx.b_left < (msgbx.b_left + 5)) {
            askbx.b_left = msgbx.b_left + 5;
          }

          /* Determine new left edge of prompt text. */
          lprompt = askbx.b_left - vfwm2;    /* lower prompt left */
          tprompt = askbx.b_left - vfwm1;    /* top prompt left */
          if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 
          asklprompt = lprompt;   /* remember the position */
          asktprompt = tprompt;

          /* Redraw the prompt strings and then the boxes. */
          if (saved_font != current_font) winfnt_(&saved_font);
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (initial_f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          altbox(alt,asklm3,asklm3+1,&msgbx.b_bottom,&altbox_left,'-');
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        if (xboxinside(askbx,x,y)){
          offsc = ((x - (askbx.b_left+f_width)) / f_width);
          x1 = (askbx.b_left+f_width) + (offsc * f_width);
/* debug  fprintf(stderr,"textfollow: x %d y %d x1 %d offsc %d %c\n",x,y,x1,offsc,sbuf[offsc]); */
          if( lstrlen == 1 ) {
            offsc = 1;
            x1 = (askbx.b_left+f_width) + (offsc * f_width);
          }
          if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
          update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
          initial_button_in = TRUE;
          break;
        } else if (xboxinside(okb,x,y)){
          okbox("ok",2,3,&msgbx.b_bottom,&okb.b_left,'!'); /* brief hilight */
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(defb,x,y)){
          dbox("d",1,2,&msgbx.b_bottom,&defb.b_left,'!'); /* brief hilight */
          *id = 1;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(altb,x,y)){
          altbox(alt,lm3,lm3+1,&msgbx.b_bottom,&altb.b_left,'!'); /* brief hilight */
          *id = 2;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(querb,x,y)){
          qbox_("?",1,2,&msgbx.b_bottom,&querb.b_left,'!'); /* brief hilight */
	  impx = 0; impy = 0; ishowmoreflg = 0; uresp = 0;
          if (help_lines <= 20) ipflg = 0;
          if (help_lines > 20) ipflg = 1;
          egphelp_(&impx,&impy,&ipflg,&ishowmoreflg,&uresp);
          break;
          } else {
          iaux = aux_menu((XEvent *) &event);   /* check and see if text scrolled etc. */
          /* redraw the dialog */ 
/* debug  fprintf(stderr,"Inside askdialog display x %d y %d\n",x,y);  */
          if (saved_font != current_font) winfnt_(&saved_font);
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          altbox(alt,asklm3,asklm3+1,&msgbx.b_bottom,&altbox_left,'-');
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
          no_valid_event = TRUE;
          break;
        }
      case KeyPress:	/* (XKeyEvent)&ev */
        if (initial_button_in) {
          track_edit_str(sbuf,&event,&x1,&f_len,&len,&fitchars,&offsc,&no_valid_event);
         }
        break;
    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }

  strncpy(sstr,sbuf,(unsigned int)f_len);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor);
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
} /* askaltdialog */


/* **************  Get string from dialogue & cancel input box *************** */
/*
 Code implements control of a single line input facility with cancel detect.
 Input begins when cursor is within askbx.
*/
void askcncldialog_(sstr,cncl,id,iq,f_len,a_len)
  char *sstr,*cncl;         /* string returned to fortran (remember strip end mark) */
  long int *id,*iq;   /* flag for default and help querry */
  int  f_len,a_len;   /* character width of the string from fortran */
{
  XEvent event;
  XWindowAttributes wa;
  XGlyphInfo info;
  XftDraw *draw;
  static char sbuf[144];
  int b_width;   /* b_width  pixels w/in box */
  int fitchars,offsc,x1,fitpix;  /* chars able to fit within box, chars between left of string & cursor */
  int	no_valid_event = TRUE;
  int	initial_button_in = FALSE;
  int x,y,len,lstrlen,lm3,iaux;
  long int impx,impy,ipflg,ishowmoreflg,uresp;
  long int saved_font;
  int okbox_left, qbox_left, dbox_left, cnclbox_left;	/* positions of small boxes */
  int initial_f_height;  /* font height used for the small boxes */
  unsigned int start_height,start_width;
  int lprompt,tprompt,tpromptgl;   /* cursor position, prompt left side */
  int vfw,vfwm1,vfwm2;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=0;

  XftTextExtents8(theDisp,fst,askmsg1,asklm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"askcncldialog askmsg1 %s asklm1 %d vwfm1 %d f_width %d timesfw %d\n",askmsg1,asklm1,vfwm1,f_width,(asklm1 * f_width));

  XftTextExtents8(theDisp,fst,askmsg2,asklm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"askcncldialog askmsg2 %s asklm2 %d vwfm2 %d f_width %d timesfw %d\n",askmsg2,asklm2,vfwm2,f_width,(asklm2 * f_width));

/* ok box is 4th box over and 8+asklm3 char total to the right */
  okbox_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + asklm3)));
/* query box is 3rd box over and 5+asklm3 char total to the right */
  qbox_left = msgbx.b_right - ((3 * 5) + (f_width * (5 + asklm3)));
/* defaults box is 2 box over and 3+asklm3 char total to the right */
  dbox_left = msgbx.b_right - ((2 * 5) + (f_width * (3 + asklm3)));
/* cncl box is on far right  */
  cnclbox_left = msgbx.b_right - ((1 * 5) + (f_width * (1 + asklm3)));
  initial_f_height = f_height;  /* remember in case of a resize */

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
/*
 Find actual string length and truncate when printing to fit within box.
*/
  strncpy(sbuf,"                                                                                               ",96);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);
  f_to_c_l(sstr,&f_len,&len); strncpy(sbuf,sstr,(unsigned int)len);
  f_to_c_l(cncl,&a_len,&lm3);

  b_width = WIDTH(askbx);
  fitchars = ((b_width - f_width) / f_width);
  XftTextExtents8(theDisp,fst,sbuf,len,&info);
  fitpix = info.width;
/* debug  fprintf(stderr,"textfollow: sbuf is %s %d %d %d %d\n",sbuf,f_len,len,fitchars,fitpix); */
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
  XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
  XFlush(theDisp);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,cross_cursor);

/* Place cursor at the end of the existing string */
  offsc = len;
  x1 = (askbx.b_left+f_width) + (offsc * f_width);
  if( lstrlen == 1 ) {
    offsc = 1;
    x1 = (askbx.b_left+f_width) + (offsc * f_width);
  }
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; } 
  update_edit_str(askbx,sbuf,&x1,&lstrlen);   /* clear and re-draw askbx box and text */
  initial_button_in = TRUE; 

  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"askcncldialog: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (initial_f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          altbox(cncl,asklm3,asklm3+1,&msgbx.b_bottom,&cnclbox_left,'-');
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {

	/* Window resized so force update of the positions of the various boxes
           including the position of askbx.
           << todo put similar logic in other dialogs >> */
        /* debug  fprintf(stderr,"askcncldialog detected configure event\n"); */
          refreshenv_();
          okbox_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + asklm3)));
          qbox_left = msgbx.b_right - ((3 * 5) + (f_width * (5 + asklm3)));
          dbox_left = msgbx.b_right - ((2 * 5) + (f_width * (3 + asklm3)));
          cnclbox_left = msgbx.b_right - ((1 * 5) + (f_width * (1 + asklm3)));

          /* Figure out where askbx is now. */
          askbx.b_right = okbox_left - 5;
          askbx.b_left = askbx.b_right -((ask_len+2) * f_width);
          askbx.b_top = msgbx.b_bottom - (initial_f_height + 6);  /* use initial font height */
          askbx.b_bottom = msgbx.b_bottom -2;
          if (askbx.b_left < (msgbx.b_left + 5)) {
            askbx.b_left = msgbx.b_left + 5;
          }

          /* Determine new left edge of prompt text. */
          lprompt = askbx.b_left - vfwm2;    /* lower prompt left */
          tprompt = askbx.b_left - vfwm1;    /* top prompt left */
          if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 
          asklprompt = lprompt;   /* remember the position */
          asktprompt = tprompt;

          /* Redraw the prompt strings and then the boxes. */
          if (saved_font != current_font) winfnt_(&saved_font);
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (initial_f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);  /* print text */
          altbox(cncl,asklm3,asklm3+1,&msgbx.b_bottom,&cnclbox_left,'-');
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        if (xboxinside(askbx,x,y)){
          offsc = ((x - (askbx.b_left+f_width)) / f_width);
          x1 = (askbx.b_left+f_width) + (offsc * f_width);
/* debug  fprintf(stderr,"textfollow: x %d y %d x1 %d offsc %d %c\n",x,y,x1,offsc,sbuf[offsc]); */
          if( lstrlen == 1 ) {
            offsc = 1;
            x1 = (askbx.b_left+f_width) + (offsc * f_width);
          }
          if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
          update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
          initial_button_in = TRUE;
          break;
        } else if (xboxinside(okb,x,y)){
          okbox("ok",2,3,&msgbx.b_bottom,&okb.b_left,'!'); /* brief hilight */
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(defb,x,y)){
          dbox("d",1,2,&msgbx.b_bottom,&defb.b_left,'!'); /* brief hilight */
          *id = 1;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(altb,x,y)){
          altbox(cncl,lm3,lm3+1,&msgbx.b_bottom,&altb.b_left,'!'); /* brief hilight */
          *id = 2;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(querb,x,y)){
          qbox_("?",1,2,&msgbx.b_bottom,&querb.b_left,'!'); /* brief hilight */
	  impx = 0; impy = 0; ishowmoreflg = 0; uresp = 0;
          if (help_lines <= 20) ipflg = 0;
          if (help_lines > 20) ipflg = 1;
          egphelp_(&impx,&impy,&ipflg,&ishowmoreflg,&uresp);
          break;
          } else {
          iaux = aux_menu((XEvent *) &event);   /* check and see if text scrolled etc. */
          /* redraw the dialog */ 
/* debug  fprintf(stderr,"Inside askdialog display x %d y %d\n",x,y);  */
          if (saved_font != current_font) winfnt_(&saved_font);
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          altbox(cncl,asklm3,asklm3+1,&msgbx.b_bottom,&cnclbox_left,'-');
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
          no_valid_event = TRUE;
          break;
        }
      case KeyPress:	/* (XKeyEvent)&ev */
        if (initial_button_in) {
          track_edit_str(sbuf,&event,&x1,&f_len,&len,&fitchars,&offsc,&no_valid_event);
         }
        break;
    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }

  strncpy(sstr,sbuf,(unsigned int)f_len);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor);
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
} /* askcncldialog */

/* **************  Get string from dialogue & 2 alt command input boxs *************** */
/*
 Code implements control of a single line input facility with 2 alt control detect.
 Input begins when cursor is within askbx.
*/
void ask2altdialog_(sstr,alt,alt2,id,iq,f_len,a_len,b_len)
  char *sstr,*alt,*alt2;         /* string returned to fortran (remember strip end mark) */
  long int *id,*iq;   /* flag for default and help querry */
  int  f_len,a_len,b_len;   /* character width of the string from fortran */
{
  XEvent event;
  XWindowAttributes wa;
  XGlyphInfo info;
  XftDraw *draw;
  static char sbuf[144];
  int b_width;   /* b_width  pixels w/in box */
  int fitchars,offsc,x1,fitpix;  /* chars able to fit within box, chars between left of string & cursor */
  int	no_valid_event = TRUE;
  int	initial_button_in = FALSE;
  int x,y,len,lstrlen,lm3,lm4,iaux;
  int vfw,vfwm1,vfwm2;
  long int impx,impy,ipflg,ishowmoreflg,uresp;
  long int saved_font;
  int okbox_left, qbox_left, dbox_left, altbox_left, alt2box_left; /* positions of small boxes */
  unsigned int start_height,start_width;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

/* Use XftTextExtents8 logic for sizing text areas. Currently code calling */
/* this will have set fixed width fonts so that the editing area works. But */
/* currently there is no provision for using proportional fonts for the prompts. */
  vfw=vfwm1=vfwm2=0;

  XftTextExtents8(theDisp,fst,askmsg1,asklm1,&info);
  if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"ask2altdialog askmsg1 %s asklm1 %d vwfm1 %d f_width %d timesfw %d\n",askmsg1,asklm1,vfwm1,f_width,(asklm1 * f_width));

  XftTextExtents8(theDisp,fst,askmsg2,asklm2,&info);
  if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//  fprintf(stderr,"ask2altdialog askmsg2 %s asklm2 %d vwfm2 %d f_width %d timesfw %d\n",askmsg2,asklm2,vfwm2,f_width,(asklm2 * f_width));

/* alt box is between ok and askbx  */
  altbox_left = msgbx.b_right - ((5 * 5) + (f_width * (9 + asklm3 + asklm4)));
/* 2nd alt box is between ok and 1st alt  */
  alt2box_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + asklm4)));
/* ok box is 3rd box over and 7 char total to the right */
  okbox_left = msgbx.b_right - ((3 * 5) + (f_width * 7));
/* query box is 2rd box over and 4 char total to the right */
  qbox_left = msgbx.b_right - ((2 * 5) + (f_width * 4));
/* defaults box is 1 box over and 2 char total to the right */
  dbox_left = msgbx.b_right - ((1 * 5) + (f_width * 2));

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
/*
 Find actual string length and truncate when printing to fit within box.
*/
  strncpy(sbuf,"                                                                                              ",96);
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);
  f_to_c_l(sstr,&f_len,&len); strncpy(sbuf,sstr,(unsigned int)len);
  f_to_c_l(alt,&a_len,&lm3);
  f_to_c_l(alt2,&b_len,&lm4);

  b_width = WIDTH(askbx);
  fitchars = ((b_width - f_width) / f_width);
  XftTextExtents8(theDisp,fst,sbuf,len,&info);
  fitpix = info.width;
/* debug  fprintf(stderr,"textfollow: sbuf is %s %d %d %d %d\n",sbuf,f_len,len,fitchars,fitpix); */
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
  XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
  XFlush(theDisp);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,cross_cursor);

/* Place cursor at the end of the existing string */
  offsc = len;
  x1 = (askbx.b_left+f_width) + (offsc * f_width);
  if( lstrlen == 1 ) {
    offsc = 1;
    x1 = (askbx.b_left+f_width) + (offsc * f_width);
  }
  if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; } 
  update_edit_str(askbx,sbuf,&x1,&lstrlen);   /* clear and re-draw askbx box and text */
  initial_button_in = TRUE; 

  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"askaltdialog: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          altbox(alt,asklm3,asklm3+1,&msgbx.b_bottom,&altbox_left,'-');
          alt2box(alt2,asklm4,asklm4+1,&msgbx.b_bottom,&alt2box_left,'-');
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"askaltdialog detected configure event\n"); */
          refreshenv_();
          altbox_left = msgbx.b_right - ((5 * 5) + (f_width * (9 + asklm3)));
          altbox(alt,asklm3,asklm3+1,&msgbx.b_bottom,&altbox_left,'-');
          alt2box_left = msgbx.b_right - ((4 * 5) + (f_width * (8 + asklm4)));
          alt2box(alt2,asklm4,asklm4+1,&msgbx.b_bottom,&alt2box_left,'-');
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
        }
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        if (xboxinside(askbx,x,y)){
          offsc = ((x - (askbx.b_left+f_width)) / f_width);
          x1 = (askbx.b_left+f_width) + (offsc * f_width);
/* debug  fprintf(stderr,"textfollow: x %d y %d x1 %d offsc %d %c\n",x,y,x1,offsc,sbuf[offsc]); */
          if( lstrlen == 1 ) {
            offsc = 1;
            x1 = (askbx.b_left+f_width) + (offsc * f_width);
          }
          if ((len+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = len; }
          update_edit_str(askbx,sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
          initial_button_in = TRUE;
          break;
        } else if (xboxinside(okb,x,y)){
          okbox("ok",2,3,&msgbx.b_bottom,&okb.b_left,'!'); /* brief hilight */
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(defb,x,y)){
          dbox("d",1,2,&msgbx.b_bottom,&defb.b_left,'!'); /* brief hilight */
          *id = 1;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(altb,x,y)){
          altbox(alt,lm3,lm3+1,&msgbx.b_bottom,&altb.b_left,'!'); /* brief hilight */
          *id = 2;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(altc,x,y)){
          alt2box(alt2,lm4,lm4+1,&msgbx.b_bottom,&altc.b_left,'!'); /* brief hilight */
          *id = 3;
          no_valid_event = FALSE;
          break;
        } else if (xboxinside(querb,x,y)){
          qbox_("?",1,2,&msgbx.b_bottom,&querb.b_left,'!'); /* brief hilight */
	  impx = 0; impy = 0; ishowmoreflg = 0; uresp = 0;
          if (help_lines <= 20) ipflg = 0;
          if (help_lines > 20) ipflg = 1;
          egphelp_(&impx,&impy,&ipflg,&ishowmoreflg,&uresp);
          break;
          } else {
          iaux = aux_menu((XEvent *) &event);   /* check and see if text scrolled etc. */
          /* redraw the dialog */ 
/* debug  fprintf(stderr,"Inside askdialog display x %d y %d\n",x,y);  */
          if (saved_font != current_font) winfnt_(&saved_font);
          altbox(alt,asklm3,asklm3+1,&msgbx.b_bottom,&altbox_left,'-');
          alt2box(alt2,asklm4,asklm4+1,&msgbx.b_bottom,&alt2box_left,'-');
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,asktprompt,msgbx.b_bottom - (f_height+8),(XftChar8 *) askmsg1,asklm1);
          XftDrawString8(draw, &xft_color,fst,asklprompt,msgbx.b_bottom - 3,(XftChar8 *) askmsg2,asklm2);
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'-');
          dbox("d",1,2,&msgbx.b_bottom,&dbox_left,'-');
          okbox("ok",2,3,&msgbx.b_bottom,&okbox_left,'-');
          xbox(askbx,fg,white,BMCLEAR |BMEDGES);   /* draw input box with edges  */
          XftDrawString8(draw, &xft_color,fst,askbx.b_left+f_width,askbx.b_bottom-3,(XftChar8 *) sbuf,lstrlen);
          XFlush(theDisp);
          no_valid_event = TRUE;
          break;
        }
      case KeyPress:	/* (XKeyEvent)&ev */
        if (initial_button_in) {
          track_edit_str(sbuf,&event,&x1,&f_len,&len,&fitchars,&offsc,&no_valid_event);
         }
        break;

    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }

  strncpy(sstr,sbuf,(unsigned int)f_len);
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor);
  XftDrawDestroy(draw);
  if (saved_font != butn_fnt) winfnt_(&saved_font);
} /* ask2altdialog */

/* **************  Write prompts in dialogue box *************** */
/*
 Passed two strings (as in USRMSG) and their lengths writes into
 dialogue box (msgbx) after clearing the box.
*/
void msgbox_(msg1,msg2,len1,len2)
  char      *msg1,*msg2;
  int  len1,len2;      /* lengths as supplied by fortran      */
{
  XftDraw *draw;
  int lm1, lm2;     /* local string lengths found by test  */
  long int saved_font;

/*
 Loop down through the string passed and see if anything is non-blank
 ie the actual end of the characters passed from fortran.
*/
  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);
  f_to_c_l(msg1,&len1,&lm1);
  f_to_c_l(msg2,&len2,&lm2);

/* Set number of dialogue line =2 to signal that the dialogue box exists */
  dialogue_lines = 2;
  msgbx.b_top = xrt_height - ((f_height+4) * 2) - 3;
  msgbx.b_bottom= xrt_height - 3;
  msgbx.b_left  = 2;
/*  msgbx.b_right = xrt_width - (f_width * 11); */
  msgbx.b_right = xrt_width - 2;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
  XftDrawString8(draw, &xft_color,fst,msgbx.b_left+2,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
  XftDrawString8(draw, &xft_color,fst,msgbx.b_left+2,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
  XFlush(theDisp);  /* added to force draw */
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  XftDrawDestroy(draw);
  return;
} /* msgbox */

/* **************  Open a dialog continue box *************** */
/*
 Passed the prompts which were sent to the dialogue box so that the
 correct position is defined. choicebx contains the prompts while
 a box is reserved for user input. If there is only a " " in the second
 prompt then position askbx against the left side.
*/
void continuebox_(msg1,msg2,opta,len1,len2,len3)
  char    *msg1,*msg2;         /* character strings for each line */
  char    *opta;         /* action box (usually continue) */
  int len1,len2,len3;     /* lengths as supplied by fortran (ignored) */
{
  XEvent event;
  XWindowAttributes wa;
  XftDraw *draw;
  XGlyphInfo info;
  int	no_valid_event = TRUE;
  int abox_left, msg_bb ;	/* positions of small boxes */
  int x1,y1,lprompt,tprompt,tpromptgl;         /* cursor position, prompt left side   */
  int lm1,lm2,lm3;         /* local string lengths found by test      */
  long int saved_font;
  unsigned int start_height,start_width;
  int iaux;         /* unused return from aux_menu      */
  int vfw;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

/* Find ends of strings passed and terminate. */
   saved_font = current_font;
   if (saved_font != butn_fnt) winfnt_(&butn_fnt);
   f_to_c_l(msg1,&len1,&lm1); f_to_c_l(msg2,&len2,&lm2);
   f_to_c_l(opta,&len3,&lm3);

/* action box is right box and 2 char total to the right */
/* XftTextExtents8 needed here */
  abox_left = msgbx.b_right - ((2 * 5) + (f_width * lm3+2));

/* determine left edge of prompt text */
  lprompt = abox_left;
  tprompt = msgbx.b_right - ((lm1+2) * f_width);
  vfw=0;
  XftTextExtents8(theDisp,fst,msg1,lm1,&info);
  if( info.xOff > vfw ) vfw= info.xOff+5;
  tpromptgl = msgbx.b_right - vfw;
  if (tprompt < lprompt) lprompt = tprompt;
  if (tpromptgl < lprompt) lprompt = tpromptgl;
  tprompt = abox_left - ((lm2+2) * f_width);
  vfw=0;
  XftTextExtents8(theDisp,fst,msg2,lm2,&info);
  if( info.xOff > vfw ) vfw= info.xOff+5;
  tpromptgl = abox_left - vfw;
  if (tprompt < lprompt) lprompt = tprompt;
  if (tpromptgl < lprompt) lprompt = tpromptgl;
  if (lprompt < msgbx.b_left) lprompt = msgbx.b_left+2;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
  XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
  XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
  XFlush(theDisp);
  msg_bb = msgbx.b_bottom;
  abcdboxs(opta,lm3,lm3+1,&msg_bb,&abox_left,'a');       /* yes box with edges  */
/*
  Check to see if mouse moves into the option a box and
  don't leave until one has been selected.
*/
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,cross_cursor);
  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"continue box: vis event %d\n",event.xvisibility.state);  */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
          XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
          XFlush(theDisp);
          msg_bb = msgbx.b_bottom;
          abcdboxs(opta,lm3,lm3+1,&msg_bb,&abox_left,'a');       /* yes box with edges  */
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"continue box detected configure event\n"); */
          refreshenv_();
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
          XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
          XFlush(theDisp);
          msg_bb = msgbx.b_bottom;
          abcdboxs(opta,lm3,lm3+1,&msg_bb,&abox_left,'a');       /* yes box with edges  */
        }
        break;
      case ButtonPress:
        x1 = event.xbutton.x;  y1 = event.xbutton.y;
        if (xboxinside(a,x1,y1)){
          no_valid_event = FALSE;
          xbox(a,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,a.b_left+3,a.b_bottom-3,(XftChar8 *) opta,lm3);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          break;
        } else {
          iaux = aux_menu((XEvent *) &event);	/* check and see if text scrolled etc. */
          if ( iaux == 2 ) {	/* if resize then redraw the dialog */
/* debug  fprintf(stderr,"Inside continuebox display\n");  */
            xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
            XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
            XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
            XFlush(theDisp);
            msg_bb = msgbx.b_bottom;
            abcdboxs(opta,lm3,lm3+1,&msg_bb,&abox_left,'a');       /* yes box with edges  */
          }
          no_valid_event = FALSE;
          break;
        }

      case KeyPress:	/* (XKeyEvent)&ev */
	     XFlush(theDisp);	/* flush event */
        no_valid_event = FALSE;
        break;
    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor); /* turn on arrow cursor */

/* Clear the dialogue box. */
  Timer(100);
  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);  /* clear dialogue box  */
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  XftDrawDestroy(draw);
  return;
} /* continuebox */


/* **************  Open a two-seven choice box *************** */
/*
 Passed the prompts which were sent to the dialogue box so that the
 correct position is defined. choicebx contains the prompts while
 a & b & c & d & e & f & g is reserved for user input. If querry box is
 selected then ok returned as 8 which should be trapped by calling code.
*/
void abcdefbox_(msg1,msg2,opta,optb,optc,optd,opte,optf,optg,ok,len1,len2,len3,len4,len5,len6,len7,len8,len9)
  char    *msg1,*msg2;		/* character strings for each line */
  char    *opta,*optb,*optc,*optd,*opte,*optf,*optg;	/* character strings for options */
  int len1,len2,len3,len4,len5,len6,len7,len8,len9;	/* lengths as supplied by fortran (ignored) */
  long int *ok;                /* returned 1=option a picked, 2=option b picked etc. */
{
  XEvent event;
  XWindowAttributes wa;
  KeySym     ks;
  XftDraw *draw;
  XGlyphInfo info;
  static char buf[80],*bp;
  char k_char,keypressed;
  int	no_valid_event = TRUE;
  int x1,y1,lprompt,tprompt;	/* cursor position, prompt left side and with glyph info  */
  int qbox_left,abox_left,bbox_left,cbox_left,dbox_left,ebox_left,fbox_left,gbox_left;	/* positions of small boxes */
  int lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,msg_bb;	/* local string lengths found by test      */
  int nopts;	/* number of options (based on if option text blank) */
  long int saved_font;
  static int blen = 0;
  unsigned int start_height,start_width;
  int iaux;         /* unused return from aux_menu      */
  int vfw,vfwa,vfwb,vfwc,vfwd,vfwe,vfwf,vfwg,vfwm1,vfwm2;  /* pixel width of each option */

/* Text within choice boxes use the butn_fnt so calling code should ensure that
   this has been reset appropriately before calling. */
   saved_font = current_font;
   if (saved_font != butn_fnt) winfnt_(&butn_fnt);

/* Use XftTextExtents8 logic for sizing each option box */
   vfw=vfwa=vfwb=vfwc=vfwd=vfwe=vfwf=vfwg=vfwm1=vfwm2=0;

/* Find ends of strings passed and terminate. Add 2 character widths
   to each vfwm* to allow a bit of white space around the word. */
   nopts = 0;
   f_to_c_l(msg1,&len1,&lm1);
   XftTextExtents8(theDisp,fst,msg1,lm1,&info);
   if( info.xOff > vfwm1 ) vfwm1= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//   fprintf(stderr,"abcdefbox msg1 %s lm1 %d vwfm1 %d f_width %d timesfw %d\n",msg1,lm1,vfwm1,f_width,(lm1 * f_width));

   f_to_c_l(msg2,&len2,&lm2);
   XftTextExtents8(theDisp,fst,msg2,lm2,&info);
   if( info.xOff > vfwm2 ) vfwm2= info.xOff + 2*f_width;  /* allow for a bit of space on right */
//   fprintf(stderr,"abcdefbox msg2 %s lm2 %d vwfm2 %d f_width %d timesfw %d\n",msg2,lm2,vfwm2,f_width,(lm2 * f_width));

   f_to_c_l(opta,&len3,&lm3);
   XftTextExtents8(theDisp,fst,opta,lm3,&info);
   if( info.xOff > vfwa ) vfwa= info.xOff + 2*f_width;  /* impose requested additional space */
//   fprintf(stderr,"abcdefbox opta %s lm3 %d vwfa %d f_width %d timesfw %d\n",opta,lm3,vfwa,f_width,(lm3 * f_width));

   f_to_c_l(optb,&len4,&lm4); if ( lm4 > 1 ) nopts = 2;
   XftTextExtents8(theDisp,fst,optb,lm4,&info);
   if( info.xOff > vfwb ) vfwb= info.xOff + 2*f_width;  /* impose requested additional space */
//   fprintf(stderr,"abcdefbox optb %s lm4 %d vwfb %d f_width %d timesfw %d\n",optb,lm4,vfwb,f_width,(lm4 * f_width));

   f_to_c_l(optc,&len5,&lm5); if ( lm5 > 1 ) nopts = 3;
   XftTextExtents8(theDisp,fst,optc,lm5,&info);
   if( info.xOff > vfwc ) vfwc= info.xOff + 2*f_width;  /* impose requested additional space */
//   fprintf(stderr,"abcdefbox optc %s lm5 %d vwfc %d f_width %d timesfw %d\n",optc,lm5,vfwc,f_width,(lm5 * f_width));

   f_to_c_l(optd,&len6,&lm6); if ( lm6 > 1 ) nopts = 4;
   XftTextExtents8(theDisp,fst,optd,lm6,&info);
   if( info.xOff > vfwd ) vfwd= info.xOff + 2*f_width;  /* impose requested additional space */
//   fprintf(stderr,"abcdefbox optd %s lm6 %d vwfd %d f_width %d timesfw %d\n",optd,lm6,vfwd,f_width,(lm6 * f_width));

   f_to_c_l(opte,&len7,&lm7); if ( lm7 > 1 ) nopts = 5;
   XftTextExtents8(theDisp,fst,opte,lm7,&info);
   if( info.xOff > vfwe ) vfwe= info.xOff + 2*f_width;  /* impose requested additional space */
//   fprintf(stderr,"abcdefbox opte %s lm7 %d vwfe %d f_width %d timesfw %d\n",opte,lm7,vfwe,f_width,(lm7 * f_width));

   f_to_c_l(optf,&len8,&lm8); if ( lm8 > 1 ) nopts = 6;
   XftTextExtents8(theDisp,fst,optf,lm8,&info);
   if( info.xOff > vfwf ) vfwf= info.xOff + 2*f_width;  /* impose requested additional space */
//   fprintf(stderr,"abcdefbox optf %s lm8 %d vwff %d f_width %d timesfw %d\n",optf,lm8,vfwf,f_width,(lm8 * f_width));

   f_to_c_l(optg,&len9,&lm9); if ( lm9 > 1 ) nopts = 7;
   XftTextExtents8(theDisp,fst,optg,lm9,&info);
   if( info.xOff > vfwg ) vfwg= info.xOff + 2*f_width;  /* impose requested additional space */
//   fprintf(stderr,"abcdefbox optg %s lm9 %d vwfg %d f_width %d timesfw %d\n",optg,lm9,vfwg,f_width,(lm9 * f_width));
   if (nopts <2) return;	/* if less than 2 choices there is a problem. */

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

/* query box is right box and 2 char total to the right */
  qbox_left = msgbx.b_right - ((f_width * 2)+3 );
/* a, b, c, d, e, f and g boxs ... */
  if (nopts == 2) {
    bbox_left = qbox_left - (vfwb+6 );
    abox_left = qbox_left - (vfwa+vfwb+12 );
  } else if (nopts == 3) {
    cbox_left = qbox_left - (vfwc+6 );
    bbox_left = qbox_left - (vfwb+vfwc+12 );
    abox_left = qbox_left - (vfwa+vfwb+vfwc+18 );
  } else if (nopts == 4) {
    dbox_left = qbox_left - (vfwd+6 );
    cbox_left = qbox_left - (vfwc+vfwd+12 );
    bbox_left = qbox_left - (vfwb+vfwc+vfwd+18 );
    abox_left = qbox_left - (vfwa+vfwb+vfwc+vfwd+24 );
  } else if (nopts == 5) {
    ebox_left = qbox_left - (vfwe+6 );
    dbox_left = qbox_left - (vfwd+vfwe+12 );
    cbox_left = qbox_left - (vfwc+vfwd+vfwe+18 );
    bbox_left = qbox_left - (vfwb+vfwc+vfwd+vfwe+24 );
    abox_left = qbox_left - (vfwa+vfwb+vfwc+vfwd+vfwe+30 );
  } else if (nopts == 6) {
    fbox_left = qbox_left - (vfwf+6 );
    ebox_left = qbox_left - (vfwe+vfwf+12 );
    dbox_left = qbox_left - (vfwd+vfwe+vfwf+18 );
    cbox_left = qbox_left - (vfwc+vfwd+vfwe+vfwf+24 );
    bbox_left = qbox_left - (vfwb+vfwc+vfwd+vfwe+vfwf+30 );
    abox_left = qbox_left - (vfwa+vfwb+vfwc+vfwd+vfwe+vfwf+36 );
  } else if (nopts == 7) {
    gbox_left = qbox_left - (vfwg+6 );
    fbox_left = qbox_left - (vfwf+vfwg+12 );
    ebox_left = qbox_left - (vfwe+vfwf+vfwg+18 );
    dbox_left = qbox_left - (vfwd+vfwe+vfwf+vfwg+24 );
    cbox_left = qbox_left - (vfwc+vfwd+vfwe+vfwf+vfwg+30 );
    bbox_left = qbox_left - (vfwb+vfwc+vfwd+vfwe+vfwf+vfwg+36 );
    abox_left = qbox_left - (vfwa+vfwb+vfwc+vfwd+vfwe+vfwf+vfwg+42 );
  }

/* determine left edge of prompt text also check via XftTextExtents8 */
  lprompt = abox_left - vfwm2;       /* lower prompt left */
  tprompt = abox_left - vfwm1;       /* top prompt left */
  if (tprompt < 2 ) tprompt = 5;     /* keep from falling off the left */ 
//  tprompt = msgbx.b_right - vfwm1;  top prompt left

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
  XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
  XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
  XFlush(theDisp);
  msg_bb = msgbx.b_bottom;
  abcdboxs(opta,lm3,lm3+2,&msg_bb,&abox_left,'a');       /* a box with edges  */
  abcdboxs(optb,lm4,lm4+2,&msg_bb,&bbox_left,'b');       /* b box with edges  */
  if(nopts >= 3) abcdboxs(optc,lm5,lm5+2,&msg_bb,&cbox_left,'c');       /* c box with edges  */
  if(nopts >= 4) abcdboxs(optd,lm6,lm6+2,&msg_bb,&dbox_left,'d');       /* d box with edges  */
  if(nopts >= 5) abcdboxs(opte,lm7,lm7+2,&msg_bb,&ebox_left,'e');       /* e box with edges  */
  if(nopts >= 6) abcdboxs(optf,lm8,lm8+2,&msg_bb,&fbox_left,'f');       /* f box with edges  */
  if(nopts >= 7) abcdboxs(optg,lm9,lm9+2,&msg_bb,&gbox_left,'g');       /* g box with edges  */
  qbox_("?",1,2,&msg_bb,&qbox_left,'-');	/* draw querry box with edges  */
  *ok = 0;                     /* assume no answer         */
/*
  Now check to see if mouse moves into the option a or option b box and
  don't leave until one has been selected.
*/
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,cross_cursor);
  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify: /* user obscured window so refresh window and dialogue */
/* debug fprintf(stderr,"abcdefbox: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
          XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
          XFlush(theDisp);
          msg_bb = msgbx.b_bottom;
          abcdboxs(opta,lm3,lm3+2,&msg_bb,&abox_left,'a');       /* a box with edges  */
          abcdboxs(optb,lm4,lm4+2,&msg_bb,&bbox_left,'b');       /* b box with edges  */
          if(nopts >= 3) abcdboxs(optc,lm5,lm5+2,&msg_bb,&cbox_left,'c');       /* c box with edges  */
          if(nopts >= 4) abcdboxs(optd,lm6,lm6+2,&msg_bb,&dbox_left,'d');       /* d box with edges  */
          if(nopts >= 5) abcdboxs(opte,lm7,lm7+2,&msg_bb,&ebox_left,'e');       /* e box with edges  */
          if(nopts >= 6) abcdboxs(optf,lm8,lm8+2,&msg_bb,&fbox_left,'f');       /* f box with edges  */
          if(nopts >= 7) abcdboxs(optg,lm9,lm9+2,&msg_bb,&gbox_left,'g');       /* g box with edges  */
          qbox_("?",1,2,&msg_bb,&qbox_left,'-');	/* draw querry box with edges  */
          *ok = 0;                     /* assume no answer         */
        }
        break;
      case ConfigureNotify: /* user resized window so refresh window and dialogue */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"abcdefbox detected configure event\n"); */
          refreshenv_();
          xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
          XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
          XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
          XFlush(theDisp);
          msg_bb = msgbx.b_bottom;
          abcdboxs(opta,lm3,lm3+2,&msg_bb,&abox_left,'a');       /* a box with edges  */
          abcdboxs(optb,lm4,lm4+2,&msg_bb,&bbox_left,'b');       /* b box with edges  */
          if(nopts >= 3) abcdboxs(optc,lm5,lm5+2,&msg_bb,&cbox_left,'c');       /* c box with edges  */
          if(nopts >= 4) abcdboxs(optd,lm6,lm6+2,&msg_bb,&dbox_left,'d');       /* d box with edges  */
          if(nopts >= 5) abcdboxs(opte,lm7,lm7+2,&msg_bb,&ebox_left,'e');       /* e box with edges  */
          if(nopts >= 6) abcdboxs(optf,lm8,lm8+2,&msg_bb,&fbox_left,'f');       /* f box with edges  */
          if(nopts >= 7) abcdboxs(optg,lm9,lm9+2,&msg_bb,&gbox_left,'g');       /* g box with edges  */
          qbox_("?",1,2,&msg_bb,&qbox_left,'-');	/* draw querry box with edges  */
          *ok = 0;                     /* assume no answer         */
        }
        break;
      case ButtonPress:
        x1 = event.xbutton.x;  y1 = event.xbutton.y;
        if (xboxinside(a,x1,y1)){
          no_valid_event = FALSE;
          xbox(a,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,a.b_left+3,a.b_bottom-3,(XftChar8 *) opta,lm3);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          *ok = 1;
   	  break;
        } else if (xboxinside(b,x1,y1)){
          no_valid_event = FALSE;
          xbox(b,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,b.b_left+3,b.b_bottom-3,(XftChar8 *) optb,lm4);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          *ok = 2;
          break;
        } else if (xboxinside(c,x1,y1) && nopts >= 3){
          no_valid_event = FALSE;
          xbox(c,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,c.b_left+3,c.b_bottom-3,(XftChar8 *) optc,lm5);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          *ok = 3;
          break;
        } else if (xboxinside(d,x1,y1) && nopts >= 4){
          no_valid_event = FALSE;
          xbox(d,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,d.b_left+3,d.b_bottom-3,(XftChar8 *) optd,lm6);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          *ok = 4;
          break;
        } else if (xboxinside(e,x1,y1) && nopts >= 5){
          no_valid_event = FALSE;
          xbox(e,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,e.b_left+3,e.b_bottom-3,(XftChar8 *) opte,lm7);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          *ok = 5;
          break;
        } else if (xboxinside(f,x1,y1) && nopts >= 6){
          no_valid_event = FALSE;
          xbox(f,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,f.b_left+3,f.b_bottom-3,(XftChar8 *) optf,lm8);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          *ok = 6;
          break;
        } else if (xboxinside(g,x1,y1) && nopts >= 7){
          no_valid_event = FALSE;
          xbox(g,fg,ginvert,BMEDGES|BMNOT|BMCLEAR); /* invert box */
          Timer(50);
          XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
          XftDrawString8(draw, &xft_color,fst,g.b_left+3,g.b_bottom-3,(XftChar8 *) optg,lm9);
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
          *ok = 7;
          break;
        } else if (xboxinside(querb,x1,y1)){
          no_valid_event = FALSE;
          qbox_("?",1,2,&msgbx.b_bottom,&qbox_left,'!');	/* brief hilight  */
          *ok = 8;
          break;
        } else {
          no_valid_event = FALSE;
          iaux = aux_menu((XEvent *) &event);	/* check and see if text scrolled etc. */
          if ( iaux == 2 ) {	/* if resize then redraw the dialog */
            xbox(msgbx,fg,white,BMCLEAR |BMEDGES);   /* draw dialogue box with edges  */
            XftDrawString8(draw, &xft_color,fst,tprompt,msgbx.b_bottom-(f_height+8),(XftChar8 *) msg1,lm1);
            XftDrawString8(draw, &xft_color,fst,lprompt,msgbx.b_bottom-3,(XftChar8 *) msg2,lm2);
            XFlush(theDisp);
            msg_bb = msgbx.b_bottom;
            abcdboxs(opta,lm3,lm3+2,&msg_bb,&abox_left,'a');       /* a box with edges  */
            abcdboxs(optb,lm4,lm4+2,&msg_bb,&bbox_left,'b');       /* b box with edges  */
            if(nopts >= 3) abcdboxs(optc,lm5,lm5+2,&msg_bb,&cbox_left,'c');       /* c box with edges  */
            if(nopts >= 4) abcdboxs(optd,lm6,lm6+2,&msg_bb,&dbox_left,'d');       /* d box with edges  */
            if(nopts >= 5) abcdboxs(opte,lm7,lm7+2,&msg_bb,&ebox_left,'e');       /* e box with edges  */
            if(nopts >= 6) abcdboxs(optf,lm8,lm8+2,&msg_bb,&fbox_left,'f');       /* f box with edges  */
            if(nopts >= 7) abcdboxs(optg,lm9,lm9+2,&msg_bb,&gbox_left,'g');       /* g box with edges  */
            qbox_("?",1,2,&msg_bb,&qbox_left,'-');	/* draw querry box with edges  */
            *ok = 0;                     /* assume no answer         */
          }
          no_valid_event = TRUE;
          break;
        }

      case KeyPress:	/* (XKeyEvent)&ev */
        blen = XLookupString((XKeyEvent*)&event,buf,80,&ks,(XComposeStatus *) NULL);
        if(blen > 0) {
          blen--;
          bp = buf;
          k_char = *bp++;
        }
        keypressed = isupper (k_char) ? tolower (k_char) : k_char;
        if ( keypressed == 'a' )*ok = 1;
        if ( keypressed == 'b' )*ok = 2;
        if ( keypressed == 'c' )*ok = 3;
        if ( keypressed == 'd' )*ok = 4;
        if ( keypressed == 'e' )*ok = 5;
        if ( keypressed == 'f' )*ok = 6;
        if ( keypressed == 'g' )*ok = 7;
        if ( *ok == 0 ) { no_valid_event = TRUE; } else { no_valid_event = FALSE; };
        break;
    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor); /* turn on arrow cursor */

/*
  If one of the choices has been made pause and then clear the dialogue box.
*/
  if(*ok <= nopts) {
    Timer(100);
    xbox(msgbx,fg,white,BMCLEAR |BMEDGES);                    /* clear dialogue box  */
  }
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  XftDrawDestroy(draw);
  return;
} /* abcdefbox */

/* ****** drscrollbar : draw scroll bar beside text feedback ********** */
void drscrollbar()
{
  int bottom, height, totsize;

/* Draw scroll box */
  scroll.b_left = disp.b_left - 9;
  scroll.b_right = disp.b_left;
  scroll.b_top = disp.b_top;
  scroll.b_bottom = disp.b_bottom;
  xbox(scroll,fg,white,BMCLEAR |BMEDGES);      /* draw box with edges  */

/* Draw visible text box - make slightly narrower than scroll box */
  scrollbar.b_left = disp.b_left - 8;
  scrollbar.b_right = disp.b_left - 1;

/* Calculate the size of the visible text wrt amount of text in buffer */
  totsize = scroll.b_bottom - scroll.b_top - 1;
  if ( edisp_index > tfb_limtty ) {
    height = (int) ((tfb_limtty * totsize)/edisp_index );
    bottom = (int) (((edisp_index-scroll_index) * totsize)/edisp_index );
    if ((bottom+height) > totsize) bottom = totsize - height;
  } else {
    height = totsize;
    bottom = 0;
  }
  scrollbar.b_bottom = disp.b_bottom - bottom - 1;
  scrollbar.b_top = scrollbar.b_bottom - height;
  xbox(scrollbar,fg,ginvert,BMCLEAR);      /* draw box */

/*  fprintf(stderr,"scroll bar out.tp %d out.bot %d sb.tp %d sb.bot %d totsize %d bottom %d height %d\n",
           scroll.b_top,scroll.b_bottom,scrollbar.b_top,scrollbar.b_bottom,totsize,bottom,height);  */

  return;
} /* drscrollbar */

/* **************  Open a text display box *************** */
/*
 Passed the character width of the main control menu, the number of
 lines of text to leave at the bottom for a dialogue box, the number
 of lines of text to be displayed within it, the display font to be
 used in subsequent calls to Egdisp.
 Returns the overall pixel width & height.
 disp_opend is a static flag so that other facilities know there is
 a text display box.
 Range checking to make sure that it does not squeeze any graphic
 display area beyond endurance.
*/
void opengdisp_(menu_char,displ_l,dialogue_l,gdw,gdh)
 long int *menu_char,*displ_l,*dialogue_l,*gdw,*gdh;
{
 int lines = (int) *displ_l; /* page length */
 long int saved_font;       /* nominal font used within rest of application.  */
 int udh;		/* updown height */
 long int xt,yt,xb,yb; /* centre for up/down symbols  */
 long int sym,sz;      /* symbol and symbol size      */
 int  bottom,left;	/* ll position of capture box */
 int label_ht,label_wid,mf_width;     /* box label height and width of menu characters */

  saved_font = current_font;
  winfnt_(&menu_fnt);  /* menu font to get right side of box.  */
  mf_width = f_width;
  winfnt_(&butn_fnt);
  label_ht = f_height+4;
  label_wid = f_width;
  winfnt_(&butn_fnt);  /* button font (used for dialogue text).  */
  disp_opened = 1;                 /* signal that display box exists */
  disp_lines = lines;              /* signal number of available lines */
  disp.b_bottom= xrt_height - ((f_height+4) * (*dialogue_l)) - (label_ht + 7);
  disp.b_left  = 18;
  disp.b_right = xrt_width - (mf_width * (*menu_char)) - 16;

/* sort out boxs along the horizontal line between graphics and text feedback boxes */
  winfnt_(&butn_fnt);
  wire_left = disp.b_right - (f_width * 28);
  capture_left = disp.b_right - (f_width * 10);  /* bit more space */
  captext_left = disp.b_right - (f_width * 10);  /* bit more space */
  elevplus_left = disp.b_right - (f_width * 31);
  elevminus_left = disp.b_right - (f_width * 34);
  elev_left = disp.b_right - (f_width * 46);
  aziplus_left = disp.b_right - (f_width * 50);
  aziminus_left = disp.b_right - (f_width * 53);
  azi_left = disp.b_right - ((f_width * 63)+4);
  udh = f_height + 2;

  winfnt_(&disp_fnt);	/* Reload the text display font. */
/*
 Make sure disp.b_top is at least 40 pixels below the top of the window.
 If truncation pass back the number of lines allowed.
*/
 if ((disp.b_bottom -((f_height+1) * lines)) <= 50) {
     disp.b_top = 50;
     *displ_l = (int) ((disp.b_bottom - disp.b_top) / (f_height+1));
     disp_lines = (int) *displ_l;        /* update no of available lines */
 } else {
     disp.b_top  = disp.b_bottom -((f_height+1) * lines) - 10;
 }

 winfnt_(&butn_fnt);

/* draw scroll bar box */
  drscrollbar();

/* create the updown box */
 updown_text.b_bottom = disp.b_top -2;
 updown_text.b_top = updown_text.b_bottom - udh;
 updown_text.b_left =  disp.b_right - (f_width * 72);
 updown_text.b_right = updown_text.b_left + 30;
 xt = updown_text.b_left+10;         /* points for arrows */
 xb = updown_text.b_left+20;        /* points for arrows */
 yt = yb = updown_text.b_bottom - (f_height/2);
 xbox(updown_text,fg,white,BMCLEAR |BMEDGES);   /*   draw updown box with edges  */
 sym=1; sz=0;
 esymbol_(&xt,&yt,&sym,&sz);          /* upper arrow  */
 sym=2; sz=0;
 esymbol_(&xb,&yb,&sym,&sz);          /* lower arrow  */
 winfnt_(&saved_font);  /* Restore font.  */

/* include image control button just in from right edge */
 if(wire_avail >= 1) {
   bottom = disp.b_top; left = wire_left;
   doitbox(wire,"image control",13,15,&saved_font,&box_fnt,&bottom,&left,"wire",'-');
 }

/* include capture button to right of image control button */
 if(capture_avail >= 1) {
   bottom = disp.b_top; left = capture_left;
   doitbox(capture,"capture",7,9,&saved_font,&box_fnt,&bottom,&left,"capture",'-');
   bottom = fbb.b_bottom; left = captext_left;
   doitbox(capture,"capture",7,9,&saved_font,&box_fnt,&bottom,&left,"captext",'-');
 }

/* include azimuth button */
 if(azi_avail >= 1) {
   bottom = disp.b_top; left = aziplus_left;
   dosymbox(aziplus,2,&saved_font,&box_fnt,&bottom,&left,"aziplus",'-');

   bottom = disp.b_top; left = aziminus_left;
   dosymbox(aziminus,2,&saved_font,&box_fnt,&bottom,&left,"aziminus",'-');

   bottom = disp.b_top; left = azi_left;
   doitbox(azi,"azimuth",7,9,&saved_font,&box_fnt,&bottom,&left,"azi",'-');

   bottom = disp.b_top; left = elevplus_left;
   dosymbox(elevplus,2,&saved_font,&box_fnt,&bottom,&left,"elevplus",'-');

   bottom = disp.b_top; left = elevminus_left;
   dosymbox(elevminus,2,&saved_font,&box_fnt,&bottom,&left,"elevminus",'-');

   bottom = disp.b_top; left = elev_left;
   doitbox(elev,"elevation",9,11,&saved_font,&box_fnt,&bottom,&left,"elev",'-');
 }

  xbox(disp,fg,white,BMCLEAR |BMEDGES);      /* draw outer box with edges  */
  winfnt_(&butn_fnt);  /* restore button font */

 *gdh= disp.b_bottom - disp.b_top; /* text display box pixel height  */
 *gdw= disp.b_right - disp.b_left; /* text display box pixel width   */

  winfnt_(&saved_font);  /* restore std font */
  return;
} /* opengdisp */


/* *************** display text *************** */
/*
 disptext displays the text in the text display box defined in opengdisp.
 The string passed will be displayed on a single line
 however if it will  not fit in the box it will be truncated.
*/
void disptext()
{
  XftDraw *draw;
  int iy,lm1,i,len;
  long int saved_font;
  int j,jstart; 	/* variables for text feedback redisplay */
  char msg2[145];

  saved_font = current_font;   /* save existing font  */
  if (disp_fnt != saved_font) winfnt_(&disp_fnt);

  len = 144; /* max char width to print */
  disp_lines = (int) ((disp.b_bottom - disp.b_top) / (f_height+1));
  if (disp_lines != tfb_limtty) {
    tfb_line = tfb_line - tfb_limtty + disp_lines;
    tfb_limtty = disp_lines;
  }
/*  fprintf(stderr,"spad values limit %d limtty %d line %d disp_lines %d edisp_index %d\n",
           tfb_limit,tfb_limtty,tfb_line,disp_lines,edisp_index);  */
  j = 0;
  jstart = scroll_index - tfb_line;
  if (jstart < 1) jstart = 1;

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  xbox(disp,fg,white,BMCLEAR);		/* Clear area under text. */
  for ( i = jstart; i < (scroll_index +1); i++ ) {
    strcpy(msg2,edisp_list[i]);	/* copy to local */
    f_to_c_l(msg2,&len,&lm1);  if ( lm1 < len ) msg2[lm1] = '\0';
    if (((disp.b_right - 15) - disp.b_left) < ((lm1+1) * f_width)) {
      lm1 = (((disp.b_right - 15) - disp.b_left) / f_width)-1;
    }
    iy = disp.b_top + 1 + ((f_height+1) * (j + 1));
    XftDrawString8(draw, &xft_color,fst,disp.b_left+5,iy,(XftChar8 *) msg2,lm1);
    j = j + 1;
  }

  drscrollbar();		/* Update the scroll bar. */

  if (disp_fnt != saved_font) winfnt_(&saved_font);  /* Restore font.  */
  XFlush(theDisp); /* added to force draw */
  XftDrawDestroy(draw);
  return;
} /* disptext */

/* *************** text display box *************** */
/*
 egdisp takes a string from f77 and attempts to display
 it in the text display box.
*/
void egdisp_(msg,line,len)
  char  *msg;                 	 /* character string  */
  int len;              	 /* length from f77   */
  long int *line;             	 /* position indicator */
{
  int i;		 /* local string length */
  char msg2[145];

  if( len <= 1 )return; /* don`t bother if no characters */

  /* move msg to msg2, and null-terminate --- otherwise strcpy may
     do weird and not-so-wonderful things.
     Note:
       - strncpy can deal with non-null-terminated strings.
       - len <= 124 in all cases
     use of strncpy is broublesome on gcc4
  */
  // strncpy(msg2,msg,len);
  // msg2[len+1] = '\0';

/* add message to the queue */
  if(edisp_index < EDISP_LIST_LEN-1) {
    edisp_index++;
    strncpy(edisp_list[edisp_index],msg,len);	/* copy to static array */
  } else {
    for ( i = 0; i < EDISP_LIST_LEN-1; i++ ) {
      strncpy(edisp_list[i],edisp_list[i+1],144);	/* shift array up */
    }
    strncpy(edisp_list[EDISP_LIST_LEN-1],msg,len);   /* copy to static array */
  }
  scroll_index = edisp_index;		/* force scroll bar to bottom, so new line is visible */
  disptext();
  return;
} /* egdisp  */

/* ******* passes current state of spad common block (to help refresh of text feedback) */
void espad_(llimit,llimtty,lline)
  long int *llimit,*llimtty,*lline;   /* same as spad common block */
{
  tfb_limit = (int) *llimit; tfb_limtty = (int) *llimtty; tfb_line = (int) *lline;
  return;
}
/* NEXT 2 FUNCTIONS COMMENTED OUT */

/* *************** ESRU > DH. ***************
   instantiate pipe input and output channels.
 */

/* link in one of the various command.c handlers depending on the application */
/* extern void (*command)();  declare as "void command(char *, int *)" */

/* setdh_(channelin,channelout)  */
/*   long int *channelin, *channelout;    pipe in and out   */
/* {  */
/*   pipin  = *channelin;  */
/*   pipout = *channelout;  */
/*   ipcommands(pipin,command);     set handler up  */
/* }  */

/*
 todh takes a string from f77 and attempts to send it
 out channel ?? to the dialogue hander.
 The string passed will be displayed on a single line
 however if it will  not fit in the box it will be truncated.
 */

/* todh_(concept,msg,lenc,lenm)  */
/*   char  *concept, *msg;               character string  */
/*   long int *lenc, *lenm;                message lengths from f77   */
/* {
  static char	tmpstr[124];
  sprintf(tmpstr,"%s\t%s\n",concept,msg);
  write(pipout,tmpstr,(int)strlen(tmpstr));

 */

/* ******  Routine to clear text display box. ********** */
/*
 Clears text display box.
*/
void egdispclr_()
{
 xbox(disp,fg,white,BMCLEAR |BMEDGES);	/* clear outer box */
 return;
}

/* ***** draw a pixel point **** */
void drawpoint(xa,ya)
  int xa,ya;
{
  XDrawPoint(theDisp,win,theGC,xa,ya);
}

/* ******  Routine to track mouse in view box. ********** */
void trackview_(ichar,irx,iry)
 long int *ichar,*irx,*iry;	/* character returned, mouse position	*/
{
  XEvent event;
  XWindowAttributes wa;
  KeySym     ks;
  static char buf[80];
  int	no_valid_event;
  int	x,y;
  static int blen = 0;
  unsigned int start_height,start_width;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,cross_cursor);
  no_valid_event = TRUE;
  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"trackview: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"trackview detected configure event\n"); */
          refreshenv_();
        }
        break;
      case ButtonPress:
        *irx = x = event.xbutton.x;  *iry = y = event.xbutton.y;
        *ichar = (long int)event.xbutton.button;
        if (xboxinside(viewbx,x,y)){
          no_valid_event = FALSE;
          drawpoint(x,y);
          drawpoint(x+5,y+5);
          break;
        }
      case KeyPress:	/* (XKeyEvent)&ev */
        blen = XLookupString((XKeyEvent*)&event,buf,80,&ks,(XComposeStatus *) NULL);
        if(blen > 0) {
          *ichar = buf[0];
/* debug  fprintf(stderr,"in trackview %c \n",buf[0]); */
          no_valid_event = FALSE;
        }
    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  XUndefineCursor(theDisp,win);  XDefineCursor(theDisp,win,arrow_cursor); /* turn on arrow cursor */
  return;
}

/* ******  Routine to allow mouse control in view box without menu. ********** */
/* assumes azi_avail is 1 (i.e. view control buttons are active) */
void controlview_()
{
  XEvent event;
  XWindowAttributes wa;
  KeySym     ks;
  static char buf[80];
  int	no_valid_event;
  int	x,y,butid;
  int x_old,y_old,ifrlk,idx,idy;
  static int blen = 0;
  unsigned int start_height,start_width;
  long int saved_font;
  int bottom,left;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width;

  no_valid_event = TRUE;
  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case VisibilityNotify:
/* debug fprintf(stderr,"trackview: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
        XGetWindowAttributes(theDisp,win,&wa);
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
/* debug  fprintf(stderr,"trackview detected configure event\n"); */
          refreshenv_();
        }
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        butid = (int)event.xbutton.button;
        if (xboxinside(viewbx,x,y)){

  /* inside graphics display */
  /* act depending on which button was pressed */

  /* left mouse click */
  /* if view controls are present, enable freelook until mouse is released */
          if (butid==1) {
            if (azi_avail >=1) {
              x_old=x;
              y_old=y;
              no_valid_event = TRUE;
              while (no_valid_event) {
                XNextEvent(theDisp,&event);
                switch (event.type) {
                  case MotionNotify: /* while mouse is moving track position  */
                    x = event.xmotion.x; y = event.xmotion.y;
                    idx=x-x_old; idy=y-y_old;
                    ifrlk=1;
                    if (abs(idx)>10) {
                      chgazi_(&idx,&ifrlk);
                      x_old=x;
                    }
                    if (abs(idy)>10) {
                      chgelev_(&idy,&ifrlk);
                      y_old=y;
                    }
                    break;
                  case ButtonRelease:   /* button released so jump out of loop  */
                    idx=0;
                    ifrlk=0;
                    chgazi_(&idx,&ifrlk);
                    no_valid_event = FALSE;
                    break;
                  default:
                    no_valid_event = TRUE;
                    break;
                }
              }
            }

  /* right mouse click */
  /* if view controls are present, enable pan until mouse is released */
          } else if (butid==3) {
            if (azi_avail >=1) {
              x_old=x;
              y_old=y;
              no_valid_event = TRUE;
              while (no_valid_event) {
                XNextEvent(theDisp,&event);
                switch (event.type) {
                  case MotionNotify: /* while mouse is moving track position  */
                    x = event.xmotion.x; y = event.xmotion.y;
                    idx=x-x_old; idy=y-y_old;
                    ifrlk=1;
                    if (abs(idx)>10 || abs(idy)>10) {
                      chgpan_(&idx,&idy,&ifrlk);
                      x_old=x; y_old=y;
                    }
                    break;
                  case ButtonRelease:   /* button released so jump out of loop  */
                    idx=0; idy=0;
                    ifrlk=0;
                    chgpan_(&idx,&idy,&ifrlk);
                    no_valid_event = FALSE;
                    break;
                  default:
                    no_valid_event = TRUE;
                    break;
                }
              }
            }

  /* middle mouse click */
  /* if view controls are present, return to optimum view bounds */
          } else if (butid==2) {
            if (azi_avail>=1) {
              optview_();
              no_valid_event = TRUE;
              while (no_valid_event) {
                XNextEvent(theDisp,&event);
                switch (event.type) {
                  case ButtonRelease:   /* button released so jump out of loop  */
                    no_valid_event = FALSE;
                    break;
                  default:
                    no_valid_event = TRUE;
                    break;
                }
              }
            }

  /* mouse wheel up/down */
  /* if view controls are present, zoom in/out */
          } else if (butid==4 || butid==5) {
            if (azi_avail>=1) {
              idx=butid-3;
              chgzoom_(&idx);
              no_valid_event = TRUE;
              while (no_valid_event) {
                XNextEvent(theDisp,&event);
                switch (event.type) {
                  case ButtonRelease:   /* button released so jump out of loop  */
                    no_valid_event = FALSE;
                    break;
                  default:
                    no_valid_event = TRUE;
                    break;
                }
              }
            }
          }
        }
        no_valid_event = TRUE;
        break;

      case KeyPress:	/* (XKeyEvent)&ev */
        blen = XLookupString((XKeyEvent*)&event,buf,80,&ks,(XComposeStatus *) NULL);

/* Space bar tells us to exit this loop and return */
        if(ks==XK_space) {
          no_valid_event = FALSE;

/* Arrow keys activate rotation buttons as normal */
        } else if (ks==XK_Left || ks==XK_KP_Left) { /* left arrow pressed */
          no_valid_event = TRUE;
          saved_font = current_font; bottom = disp.b_top; left = aziminus_left;
          dosymbox(aziminus,2,&saved_font,&box_fnt,&bottom,&left,"aziminus",'!');
        } else if (ks==XK_Right || ks==XK_KP_Right) { /* right arrow pressed */
          no_valid_event = TRUE;
          saved_font = current_font; bottom = disp.b_top; left = aziplus_left;
          dosymbox(aziplus,2,&saved_font,&box_fnt,&bottom,&left,"aziplus",'!');
        } else if (ks==XK_Up || ks==XK_KP_Up) { /* up arrow pressed */
          no_valid_event = TRUE;
          saved_font = current_font; bottom = disp.b_top; left = elevplus_left;
          dosymbox(elevplus,2,&saved_font,&box_fnt,&bottom,&left,"elevplus",'!');
        } else if (ks==XK_Down || ks==XK_KP_Down) { /* down arrow pressed */
          no_valid_event = TRUE;
          saved_font = current_font; bottom = disp.b_top; left = elevminus_left;
          dosymbox(elevminus,2,&saved_font,&box_fnt,&bottom,&left,"elevminus",'!');
        }
        break;
    }
  }
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  return;
}


/* *************** ESRU line drawing routine to file. *************** */
/*
 Write eline attributes to file.
*/
void elinewwc_(x,y,operation)
  long int *x, *y, *operation;
{

/* If echo send parameters to wwc file */
   if ( wwc_ok == 1) {
     fprintf(wwc,"*eline\n");
     fprintf(wwc,"%ld %ld %ld\n",*x,*y,*operation);
   }
   return;
}


/* *************** ESRU line drawing routine. *************** */
/*
 co-ords, operation flag equiv to fwwutil parameters:
	1=move to absolute pixel coord,
	0=draw line to absolute pixel coord,
	3=move to relitive pixel coord,
	2=draw line to relitive pixel coord.
*/
void eline_(long int* x,long int* y,long int* operation)
{
  int x1,y1,op;

/* If echo send parameters to wwc file */
   if ( wwc_ok == 1) {
     fprintf(wwc,"*eline\n");
     fprintf(wwc,"%ld %ld %ld\n",*x,*y,*operation);
   }
   x1 = (int) *x;
   y1 = (int) *y;
   op = (int) *operation;

   if      (op == 0) {    /* LNDRAWABS */
     XDrawLine(theDisp,win,theGC,xold,yold,x1,y1);
     xold = x1; yold = y1;
   } else if (op == 1) {  /* LNMOVEABS */
     xold = x1; yold = y1;
   } else if (op == 2) {  /* LNDRAWREL */
     XDrawLine(theDisp,win,theGC,xold,yold,xold+x1,yold+y1);
     xold = xold+x1; yold = yold+y1;
   } else if (op == 3) {  /* LNMOVEREL */
     xold = xold+x1; yold = yold+y1;
   }
   return;
}

/* *************** User units to pixel coords. *************** */
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

/* *************** pixel coords to user units  *************** */
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

/* *************** General line plotting to wwc file. *************** */
/*
 As below, but outputs lines to wwc file without drawing them.
*/
 void etplotwwc_(ux,uy,updown,sym)
   float *ux, *uy;
   long int *updown, *sym;
 {
   float x,y;
   int isymbol,iupd,x1,x2,y1,y2;
   long int lx1,lx2,ly1,ly2,ipdis,isz,gs;

 /* If echo send parameters to wwc file */
   if ( wwc_ok == 1) {
     fprintf(wwc,"*etplot\n");
     fprintf(wwc,"%f %f %ld %ld\n",*ux,*uy,*updown,*sym);
    }
    return;
  }

/* *************** General line plotting. *************** */
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
  int isymbol,iupd,x1,x2,y1,y2;
  long int lx1,lx2,ly1,ly2,ipdis,isz,gs;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1) {
    fprintf(wwc,"*etplot\n");
    fprintf(wwc,"%f %f %ld %ld\n",*ux,*uy,*updown,*sym);
   }

  x = *ux;
  y = *uy;
  isymbol = (int) *sym;
  iupd = (int) *updown;

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
      XDrawLine(theDisp,win,theGC,x1,y1,x2,y2);  /* solid line       */
  } else if ((iupd < 0 && iupd > -100)) {
      ipdis= abs(iupd);
      edash_(&lx1,&ly1,&lx2,&ly2,&ipdis);           /* dashed line      */
  } else if ((iupd < -101 && iupd > -200)) {
      ipdis= abs(iupd)-100;
      echain_(&lx1,&ly1,&lx2,&ly2,&ipdis);          /* chain line       */
  } else if ((iupd < -201 && iupd > -300)) {
      ipdis= abs(iupd)-200;
      edline_(&lx1,&ly1,&lx2,&ly2,&ipdis);          /* dotted line      */
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
    case 32: gs=33;  break;  /* solid 4x4 square */
    case 33: gs=34;  break;  /* crack flow */
    case 34: gs=35;  break;  /* bidir flow */
    case 35: gs=36;  break;  /* orifice flow */
    default: gs=-1;  break;  /* nothing */
  }
  if(gs >= 0)esymbol_(&lx2,&ly2,&gs,&isz);
  wwc_macro = 0;    /* if echoing commands, turn back on */

  xold = x2;     /* remember last point. */
  yold = y2;
  return;
}

/* *************** Dotted line drawing to file. *************** */
/*
 Write edline attributes to file.
*/
void edlinewwc_(x1,y1,x2,y2,ipdis)
  long int *x1, *y1, *x2, *y2, *ipdis;
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edline\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }
  return;
}

/* *************** Dotted line drawing routine. *************** */
/*
 This function is passed both sets of pixel co-ords.
 The ipdis is the number of pixels between each dot.  Typical values
 are 2 for a dense dotted line, 3 for a sparce one, values over 4 are
 probably not useful.
*/
void edline_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{
  int ir,itrat,ix,iy,ldis,ldash;
  float xd,yd,r1,r2,xx1,xx2,yy1,yy2,xx3,yy3;
  double crow;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edline\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }

  ldash = (int) *ipdis;
  ix = (int) *x1;         /* first point in case of short line */
  iy = (int) *y1;
  xx1 = (float) *x1;  xx2 = (float) *x2;  /* use floats for calcs */
  yy1 = (float) *y1;  yy2 = (float) *y2;

/* Begin with finding out the length of the line. */
  crow = (double) (( *x1 - *x2 ) * ( *x1 - *x2) + ( *y1 - *y2) * ( *y1 - *y2));
  ldis = (int) sqrt(crow);

/*  The number of dots in the line. */
  itrat = (int) (sqrt(crow)/ (float) ldash );

/* if distance shorter than ldash then only draw point @ start. */
  if(ldash > ldis) {
    XDrawPoint(theDisp,win,theGC,ix,iy);
  } else {                    /* do dotted line */
    r2 = (float) itrat - 1.;  /* figure stepwise offset (via ratio calc) */
    r1 = 1.;
    xx3 = ((r2 * xx1) + (r1 * xx2))/ (float) itrat;
    yy3 = ((r2 * yy1) + (r1 * yy2))/ (float) itrat;
    xd = xx3 - xx1;  yd = yy3 - yy1;	/* x & y axis step change */
    ir = 0;
    while(ir < itrat) {
      ir++;
      xx1 = xx1 + xd; yy1 = yy1 + yd;	/* increment point location */
      ix = (int) xx1; iy = (int) yy1;
      XDrawPoint(theDisp,win,theGC,ix,iy);   /* plot point   */
    }
  }
  return;
}

/* ***** draw a line of width 2 between two points **** */
void drawdwl(xa,ya,xb,yb)
  int xa,ya,xb,yb;
{
  int width = 2;
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
  XDrawLine(theDisp,win,theGC,xa,ya,xb,yb);
  return;
}

/* *************** ESRU double width line to file. *************** */
/*
 Writes edwine attributest to file.
*/
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

/* *************** ESRU double width line drawing routine. *************** */
/*
 Draws a two pixel wide line between two pixel coordinates.
 Based on the slope of the line an adjacent line is drawn one pixel away.
*/
void edwline_(x1,y1,x2,y2)
  long int *x1, *y1, *x2, *y2;
{
  int ix,iy,jx,jy;
  int width = 2;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edwline\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x1,*y1,*x2,*y2);
  }

  ix = (int) *x1;  iy = (int) *y1;     /* convert to local variables */
  jx = (int) *x2;  jy = (int) *y2;
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
  XDrawLine(theDisp,win,theGC,ix,iy,jx,jy);
  return;
}


/* *************** ESRU single width line to file. *************** */
/*
 Writes eswline attributes to file.
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

/* *************** ESRU single width line drawing routine. *************** */
/*
 Draws a single pixel wide line between two pixel coordinates.
*/
void eswline_(x1,y1,x2,y2)
  long int *x1, *y1, *x2, *y2;
{
  int ix,iy,jx,jy;
  int width = 1;

  if ( wwc_ok == 1 && wwc_macro != 1) {	/* If echo send parameters to wwc file */
    fprintf(wwc,"*eswline\n");
    fprintf(wwc,"%ld %ld %ld %ld\n",*x1,*y1,*x2,*y2);
  }
  ix = (int) *x1;  iy = (int) *y1;     /* convert to local variables */
  jx = (int) *x2;  jy = (int) *y2;
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
  XDrawLine(theDisp,win,theGC,ix,iy,jx,jy);	/* draw the line */
  return;
}

/* ***** draw a line of user defined width 2 between two points **** */
void drawvwl(xa,ya,xb,yb,width)
  int xa,ya,xb,yb,width;
{
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);
  XDrawLine(theDisp,win,theGC,xa,ya,xb,yb);
}


/* *************** ESRU dashed line drawing to file. *************** */

void edashwwc_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edash\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }
  return;
}

/* *************** ESRU dashed line drawing routine. *************** */
/*
 Edash draws a dashed line based on start and end point pixel co-ords.
 Ipdis is the number of pixels between each dot.  Typical values
 are 2 for a dense dashed line, 3 for a normal one, values over 4 are
 probably not useful. Two static values are used:
   dash_on : to say whether current increment is part of a dash,
   dash_rem: to give the pixel distance remainder.

 dash_on and dash_rem are cleared if edash call is made with ipdis = 0.

*/
void edash_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{
  int ir1,ir2,ix1,iy1,ix2,iy2;
  int ix,iy,ixl,iyl,ix3,iy3,ldash,lrem,ldis;
  int width = 1;
  float trat,r,r1,r2,rinc;
  double crow;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*edash\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }

  ldash = (int) *ipdis;
  ix1 = ixl = (int) *x1;         /* give values to local variables */
  iy1 = iyl = (int) *y1;
  ix2 = (int) *x2;
  iy2 = (int) *y2;

  if(ldash == 0) {    /* clear dash_on & dash_rem and return */
    dash_on = 0;
    dash_rem = 0;
    return;
  }

/* find full length of the line. */
  crow = (double) ((ix1-ix2)*(ix1-ix2) + (iy1-iy2)*(iy1-iy2));
  ldis = (int) sqrt(crow);
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);

/* dots in line taking into account carry-over. << itrat not used >> */
  trat = (float) (ldis-dash_rem) / (float) ldash;
  lrem = (ldis-dash_rem)%ldash;       /* remaining pixels in line. */

  if(dash_rem >= 1) {                   /* If carry over get portion to draw */
    if (dash_rem > ldis) {

/*
 If remainder > segment length draw segment and subtract segment lenght
 from the remainder and then return without flipping status.
*/
      if(dash_on == 1)XDrawLine(theDisp,win,theGC,ixl,iyl,ix2,iy2);

      dash_rem =ldis-dash_rem;
      return;
    }
    ir2 = ldis - dash_rem;
    ir1 = dash_rem;
    ix3 = ((ir2 * ix1) + (ir1 * ix2))/ldis;
    iy3 = ((ir2 * iy1) + (ir1 * iy2))/ldis;

    if(dash_on == 1)XDrawLine(theDisp,win,theGC,ixl,iyl,ix3,iy3); /* draw non-trivial remainder */
    ixl = ix3;                                /* remember point  */
    iyl = iy3;
  }

/* do dashed line */
  if(dash_rem >= 1) {               /* If carry over redefine the increment */
    rinc = 1.+((float)dash_rem/(float)ldis);
  } else {
    rinc = 1.;
  }

  r = 0.;
  while(r+rinc <= trat) {
    dash_on++;                     /* flip status    */
    if(dash_on > 1) dash_on = 0;

    r=r+rinc;
    rinc=1.;         /* reset to standard increment */
    r2 = trat - r;
    r1 = r;
    ix = ((r2 * (float)ix1) + (r1 * (float)ix2))/trat;
    iy = ((r2 * (float)iy1) + (r1 * (float)iy2))/trat;
    if(dash_on == 1)XDrawLine(theDisp,win,theGC,ixl,iyl,ix,iy);

    ixl = ix;  /* remember point for next pass */
    iyl = iy;
  }
/*
 If not quite to the end then complete the fraction of the dash
 and remember portion of dash to be carried over.
*/
  if(lrem >= 1 || dash_rem >= 1) {      /* finish if non-trivial */
    dash_rem =ldash-lrem;             /* carry over */
    if(dash_rem < 0)dash_rem=0;
    dash_on++;                        /* flip status           */
    if(dash_on > 1) dash_on = 0;
    if(dash_on == 1)XDrawLine(theDisp,win,theGC,ixl,iyl,ix2,iy2);  /* complete frac of dash */
  }
  return;
}


/* *************** ESRU chained line drawing routine. *************** */
/*
 Echainwwc writes chained line instructions to file.
*/
void echainwwc_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*echain\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }
  return;
}

/* *************** ESRU chained line drawing routine. *************** */
/*
 Echain is passed start and end point pixel co-ords.
 The ipdis is the number of pixels between each dot.  Typical values
 are 3 for a dense chain, 4 for a normal one, values over 4 are
 probably not useful. Two static values are used:
   dash_on : to say whether current increment is part of a dash,
   dash_rem: to give the pixel distance remainder.

 dash_on and dash_rem are cleared if edash call is made with ipdis = 0.

*/
void echain_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis)
{
  int ir1,ir2,ix1,iy1,ix2,iy2;
  int ix,iy,ixl,iyl,ix3,iy3,ldash,lrem,ldis;
  int width = 1;
  float trat,r,r1,r2,rinc;
  double crow;

/* If echo send parameters to wwc file */
  if ( wwc_ok == 1 && wwc_macro != 1) {
    fprintf(wwc,"*echain\n");
    fprintf(wwc,"%ld %ld %ld %ld %ld\n",*x1,*y1,*x2,*y2,*ipdis);
  }

  ldash = (int) *ipdis;
  ix1 = ixl = (int) *x1;         /* give values to local variables */
  iy1 = iyl = (int) *y1;
  ix2 = (int) *x2;
  iy2 = (int) *y2;

  if(ldash == 0) {    /* clear dash_on & dash_rem and return */
    dash_on = 0;
    dash_rem = 0;
    return;
  }

/* find length of the line. */
  crow = (double) ((ix1-ix2)*(ix1-ix2) + (iy1-iy2)*(iy1-iy2));
  ldis = (int) sqrt(crow);
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);

/* dots in line taking into account carry-over. */
  trat = (float) (ldis-dash_rem) / (float) ldash;
  lrem = (ldis-dash_rem)%ldash;       /* remaining pixels in line. */

  if(dash_rem >= 1) {                   /* If carry over get portion to draw */
    if (dash_rem > ldis) {

/*
 If remainder > segment length draw segment and subtract segment lenght
 from the remainder and then return without flipping status.
*/
      if(dash_on == 1)XDrawLine(theDisp,win,theGC,ixl,iyl,ix2,iy2);
      dash_rem =ldis-dash_rem;
      return;
    }
    ir2 = ldis - dash_rem;
    ir1 = dash_rem;
    ix3 = ((ir2 * ix1) + (ir1 * ix2))/ldis;
    iy3 = ((ir2 * iy1) + (ir1 * iy2))/ldis;

    if(dash_on == 1)XDrawLine(theDisp,win,theGC,ixl,iyl,ix3,iy3); /* draw non-trivial remainder */
    ixl = ix3;                                /* remember point  */
    iyl = iy3;
  }

/* do chained line */
  if(dash_rem >= 1) {               /* If carry over redefine the increment */
    rinc = 1.+((float)dash_rem/(float)ldis);
  } else {
    rinc = 1.;
  }

  r = 0.;
  while(r+rinc <= trat) {
    dash_on++;                     /* flip status    */
    if(dash_on > 1) dash_on = 0;

    r=r+rinc;
    rinc=1.;         /* reset to standard increment */
    r2 = trat - r;
    r1 = r;
    ix = ((r2 * (float)ix1) + (r1 * (float)ix2))/trat;
    iy = ((r2 * (float)iy1) + (r1 * (float)iy2))/trat;

    if(dash_on == 1) {
      XDrawLine(theDisp,win,theGC,ixl,iyl,ix,iy);
    } else if (dash_on == 0) {
      ix3 = (ixl + ix)/2;
      iy3 = (iyl + iy)/2;
      XDrawPoint(theDisp,win,theGC,ix3,iy3);  /* if open portion then draw dot */
    }
    ixl = ix;  /* remember point for next pass */
    iyl = iy;
  }

/*
 If not quite to the end then complete the fraction of the dash
 and remember portion of dash to be carried over.
*/
  if(lrem >= 1 || dash_rem >= 1) {      /* finish if non-trivial */
    dash_rem =ldash-lrem;             /* carry over */
    if(dash_rem < 0)dash_rem=0;
    dash_on++;                        /* flip status           */
    if(dash_on > 1) dash_on = 0;

    if(dash_on == 1) {              /* complete frac of dash */
      XDrawLine(theDisp,win,theGC,ixl,iyl,ix2,iy2);
    } else if (dash_on == 0) {
      if (dash_rem >= ldash/2) {    /* if more than half of dash left */
        ir2 = dash_rem - ldash/2;   /* plot dot in this part          */
        ir1 = ldash/2;
        ix3 = ((ir2 * ixl) + (ir1 * ix2))/dash_rem;
        iy3 = ((ir2 * iyl) + (ir1 * iy2))/dash_rem;
        XDrawPoint(theDisp,win,theGC,ix3,iy3);
      }
    }
  }
  return;
}

/* ***** draw an double dashed line of user defined width between two points **** */
/* note: this leaves the gap with the underlying window colour (not necessarily the
 *       current bg colour */
void drawddash(xa,ya,xb,yb,width)
  int xa,ya,xb,yb,width;
{
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineDoubleDash,CapNotLast,JoinMiter);
  XDrawLine(theDisp,win,theGC,xa,ya,xb,yb);
}

/* ***** draw an on-off dashed line of user defined width between two points **** */
void drawoodash(xa,ya,xb,yb,width)
  int xa,ya,xb,yb,width;
{
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineOnOffDash,CapNotLast,JoinMiter);
  XDrawLine(theDisp,win,theGC,xa,ya,xb,yb);
}

/* *************** ESRU rectangle drawing routine. *************** */
/*
 This draws a rectangle based on the size and position in user units
 and transforms into pixel positions via static values passed into
 the C code via a previous call to linescale.
*/
void erectan_(x,y,dx,dy,dt)
  float *x, *y,*dx,*dy,*dt;
{
  float xo,yo,x1,y1,dx1,dy1,dt1,dd,xc,xs,yc,ys,ri;
  double xcd;
  int   ix,iy,ix1,iy1,ix2,iy2,ix3,iy3;
  int width = 1;
  long int   lix,liy;
  xo = *x;     /* origin x coord */
  yo = *y;     /* origin y coord */
  dx1 = *dx;   /* width (ie. x axis with no rotation) */
  dy1 = *dy;   /* height (ie. y axis with no rotation) */
  dt1 = *dt;   /* rotation in degrees */
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);

/* convert into pixels and move to origin*/
  u2pixel_(&xo,&yo,&lix,&liy);
  ix=(int) lix; iy=(int) liy;

/* find transforms (converting degrees into radians) */
  ri = 3.14159/180.0;
  xcd = (double) (ri * dt1);
  dd = dy1/dx1;
  xc = dx1 * (float) cos(xcd);
  xs = dx1 * (float) sin(xcd);
  yc = dd * xc;
  ys = dd * xs;

  x1=xo+xc; y1=yo+xs;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix1=(int) lix; iy1=(int) liy;
  XDrawLine(theDisp,win,theGC,ix,iy,ix1,iy1);

  x1=x1-ys; y1=y1+yc;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix2=(int) lix; iy2=(int) liy;
  XDrawLine(theDisp,win,theGC,ix1,iy1,ix2,iy2);

  x1=x1-xc; y1=y1-xs;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix3=(int) lix; iy3=(int) liy;
  XDrawLine(theDisp,win,theGC,ix2,iy2,ix3,iy3);
  XDrawLine(theDisp,win,theGC,ix3,iy3,ix,iy);  /* back to origin  */
  return;
}

/* *************** ESRU grey box drawing routine. *************** */
/*
 This draws a grey filled rectangle based on the size and position in user units
 and transforms into pixel positions via static values passed into
 the C code via a previous call to linescale.
*/
void egrbox_(x,y,dx,dy,gp)
  float *x, *y,*dx,*dy;
  long int *gp;
{
  float xo,yo,x1,y1,dx1,dy1;
  int   gpo,ix,iy,ix1,iy1;
  box	gbox;
  long int   lix,liy;
  xo = *x;     /* origin x coord */
  yo = *y;     /* origin y coord */
  dx1 = *dx;   /* delta width (ie. x axis) */
  dy1 = *dy;   /* delta height (ie. y axis) */
  gpo = (int) *gp;   /* grey step as in gscale */

/* convert into pixels and move to origin*/
  u2pixel_(&xo,&yo,&lix,&liy);
  ix=(int) lix; iy=(int) liy;

  x1 = xo + dx1; y1 = yo + dy1;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix1=(int) lix; iy1=(int) liy;

  gbox.b_bottom = iy; gbox.b_top = iy1;
  if((gbox.b_bottom-gbox.b_top)<=2)return; /* if no height return */
  gbox.b_left = ix; gbox.b_right = ix1;
  if((gbox.b_right-gbox.b_left)<=2)return; /* if no width return */
  if (gpo >= 0 && gpo < ngscale) {
    xbox(gbox,fg,gscale[gpo], BMCLEAR | BMNOT | BMEDGES);
  }else if(gpo >= 2 && gpo < 14) {
    xbox(gbox,fg,gmodbg, BMCLEAR | BMNOT | BMEDGES);
  }else if(gpo >= 14 && gpo < 36) {
    xbox(gbox,fg,gfeedfr, BMCLEAR | BMNOT | BMEDGES);
  }else if(gpo >= 36) {
    xbox(gbox,fg,black, BMCLEAR | BMNOT | BMEDGES);
  }
  return;
}

/* *************** ESRU triangle drawing routine. *************** */
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
  float xo,yo,x1,y1,dx1,dy1,dt1,dd,xc,xs,yc,ys,ri;
  double xcd;
  int   ix,iy,ix1,iy1,ix2,iy2;
  int width = 1;
  long int   lix,liy;
  xo = *x;     /* origin x coord */
  yo = *y;     /* origin y coord */
  dx1 = *dx;   /* width (ie. x axis with no rotation) */
  dy1 = *dy;   /* height (ie. y axis with no rotation) */
  dt1 = *dt;   /* rotation in degrees */
  XSetLineAttributes(theDisp,theGC,(unsigned int)width,LineSolid,CapNotLast,JoinMiter);

/* convert into pixels and move to origin*/
  u2pixel_(&xo,&yo,&lix,&liy);
  ix=(int) lix; iy=(int) liy;

/* find transforms (converting degrees into radians) */
  ri = 3.14159/180.0;
  xcd = (double) (ri * dt1);
  xc = dx1 * (float) cos(xcd);
  xs = dx1 * (float) sin(xcd);
  dd = dy1/dx1;
  yc = dd * xc;
  ys = dd * xs;

  x1=xo+xc;  y1=yo+xs;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix1=(int) lix; iy1=(int) liy;
  XDrawLine(theDisp,win,theGC,ix,iy,ix1,iy1);

  x1=x1-ys;  y1=y1+yc;
  u2pixel_(&x1,&y1,&lix,&liy);
  ix2=(int) lix; iy2=(int)liy;
  XDrawLine(theDisp,win,theGC,ix1,iy1,ix2,iy2);
  XDrawLine(theDisp,win,theGC,ix2,iy2,ix,iy);  /* back to origin  */
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
  float xo,yo,xp,yp,ths1,thf1,di1,xc,yc;
  float ri,fn,thid,th,radius,xps,yps,xpss,ypss,xpi1,ypi1,xpi2,ypi2;
  double xcd;
  int   ix,iy,nseg,i,ix1,iy1,ix2,iy2;
  long int   lix,liy;

  xo   = *x;    /* start of arc x coord */
  yo   = *y;    /* start of arc y coord */
  ths1 = *ths;  /* angle (deg between positive x axis and start of arc */
  thf1 = *thf;  /* angle (deg between positive x axis and end of arch */
  nseg = (int) *num;  /* straight line segment resolution */
  di1  = *di;   /* line type 0. solid, > 0. dashed shading */
  radius = *r;  /* radius of arc in user units  */

/* find transforms (converting degrees into radians) */
  ri = 3.14159/180.0;
  fn=1.0/(float)nseg;
  thid=fn*(thf1-ths1);
  xcd = (double) (ri * ths1);
  xc = xo - radius * (float) cos(xcd);
  yc = yo - radius * (float) sin(xcd);
  xps=xo;
  yps=yo;

/* convert into pixels and move to origin*/
  u2pixel_(&xo,&yo,&lix,&liy);
  ix = lix; iy = liy;

  for (i=1; i<nseg; i++) {
    th=ths1+(float)i*thid;
    xcd = (double) (ri * th);
    xp  = xc + radius * (float) cos(xcd);
    yp  = yc + radius * (float) sin(xcd);
    if (di1 > 0.0) {
        xpss=xp-xps;
        ypss=yp-yps;
        xpi1=xps+0.25*xpss;
        ypi1=yps+0.25*ypss;
        xpi2=xps+0.75*xpss;
        ypi2=yps+0.75*ypss;
        u2pixel_(&xpi1,&ypi1,&lix,&liy); /* move to midpoint */
        ix1 = lix; iy1 = liy;
        u2pixel_(&xpi2,&ypi2,&lix,&liy);
        ix2 = lix; iy2 = liy;
        XDrawLine(theDisp,win,theGC,ix1,iy1,ix2,iy2);
        xps=xp;
        yps=yp;
    } else if (di1 == 0.0) {
        u2pixel_(&xp,&yp,&lix,&liy);
        ix1 = lix; iy1 = liy;
        XDrawLine(theDisp,win,theGC,ix,iy,ix1,iy1);
        ix = ix1; iy = iy1;
    }
  }
}

/* *************** ESRU circle drawing routine. *************** */
/*
 co-ords, operation flag:
	0=draw circle is drawn with clear inside.
	1=draw filled black circle.
	2=to be added...
	3=to be added...
 passed the x,y centre and the radius, this is converted into position
 of the upper left corner of the bounding box (ul & ut) and the
 bounding box width and height is rad1 * 2. Note that setting zone
 or greyscale colours essentially has no impact because the black
 circle fill will always be black.
*/
void ecirc_(x,y,rad,operation)
  long int *x, *y, *rad, *operation;
{
  int x1,y1,rad1,op,ul,ut,boxdim;
  x1 = (int) *x;
  y1 = (int) *y;
  rad1 = (int) *rad;
  op = (int) *operation;
  ul = x1 - rad1; /* left edge */
  ut = y1 - rad1; /* top edge */
  boxdim = rad1 + rad1;

  if (op == 0) {
    XDrawArc(theDisp,win,theGC,ul,ut,(unsigned int)boxdim,(unsigned int)boxdim,0, 360*64);
  } else if (op == 1) {
    XSetForeground(theDisp,theGC, fg);
    XSetBackground(theDisp,theGC, fg);
    XFillArc(theDisp,win,theGC,ul,ut,(unsigned int)boxdim,(unsigned int)boxdim,0, 360*64);
    XSetForeground(theDisp,theGC, fg);
    XSetBackground(theDisp,theGC, bg);
  }
  return;
}

/* *************** ESRU arc drawing routine. *************** */
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
  int x1,y1,rad1,op,ul,ut,boxdim,an1,an2;
  x1 = (int) *x;
  y1 = (int) *y;
  rad1 = (int) *rad;
  op = (int) *operation;
  an1 = (int) *ang1;
  an2 = (int) *ang2;
  ul = x1 - rad1; /* left edge */
  ut = y1 - rad1; /* top edge */
  boxdim = rad1 + rad1;

  if (op == 0) {
    XDrawArc(theDisp,win,theGC,ul,ut,(unsigned int)boxdim,(unsigned int)boxdim,an1*64, an2*64);
  } else if (op == 1) {
    XSetForeground(theDisp,theGC, fg);
    XSetBackground(theDisp,theGC, fg);
    XFillArc(theDisp,win,theGC,ul,ut,(unsigned int)boxdim,(unsigned int)boxdim,an1*64, an2*64);
    XSetForeground(theDisp,theGC, fg);
    XSetBackground(theDisp,theGC, bg);
  }
  return;
}

/* ************** AXSCALE ************************ */
/*
 Determine the scaling ratios for use by ww based on the axis range of
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
  float axgw, axgh, axxmn, axxmx, axymn, axymx, axxsc, axysc, axxadd, axyadd;

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

/* ************** LINESCALE *********************** */
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
   x_off = (int) *loff;
   x_add = *ladd;
   x_scale = *lscale;
   y_off = (int) *boff;
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


/* ************ Generate a tic label *************** */
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
  n1 = (int) *n;
  val1 = *val;

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


/* ************** VRTAXISDDWWC *********************** */
/*
 Write to file vertical axis attributes.
*/

void vrtaxisddwwc_(float* ymn,float* ymx,long int* offl,long int* offb,long int* offt,
	float* yadd,float* sca,long int* mode,float *dddy, long int *nny,long int* side,char* msg,int mlen)
{
 int ilen;
 char msg2[80];

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


/* ************** VRTAXISDD *********************** */
/*
 Construct and draw a vertical axis via where: YMN,YMX are the data
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
 XftDraw *draw;
 int s_0,s_1, s_2, s_3, s_4, s_5;
 int ofl,ofb,oft,sid;
 char sstr[10], buf[2];
 int l, n, ix, vertadj, iy, ilen, nintvl;
 int iy1, ix1;
 int last_label_pixel, label_width, mid;
 long int ny,wticc,mde,saved_font;
 float yticv,ddy,rintvl,resid;
 char msg2[80];

 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';
 saved_font = current_font;
 if (saved_font != butn_fnt) winfnt_(&butn_fnt);

/* If echo send parameters to wwc file */
 if ( wwc_ok == 1) {
    fprintf(wwc,"*vrtaxis\n");
    fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld %ld\n",
		*ymn,*ymx,*offl,*offb,*offt,*yadd,*sca,*mode,*side);
    fprintf(wwc,"%s\n",msg2);
 }

 ofl = (int) *offl; ofb = (int) *offb; oft = (int) *offt;
 ny = (int) *nny; ddy = *dddy;
 mde = *mode; sid = (int) *side;

/* Find the maximum label text width.  */
 label_width = 0;
 yticv = *ymn;
 labelstr(&ny, &yticv, &wticc, sstr);
 label_width = (int) strlen(sstr);
 yticv = *ymx;
 labelstr(&ny, &yticv, &wticc, sstr);

/* Find the length of the axis label passed */
 if( (int) strlen(sstr) > label_width) label_width = (int) strlen(sstr);

// Define local drawable for Xft font.
 draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

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
 if (mde == 1) {
   resid = *ymn - (int) *ymn;
   if(*ymn < 0. && resid != 0.) {
       yticv = *ymn;
       iy = ofb - (int) (((float) yticv + *yadd) * *sca);
       XDrawLine(theDisp,win,theGC,ofl,ofb,s_0,iy);
   } else if(*ymn > 0. && resid != 0.) {
       yticv = (*ymn + ddy);
       iy = ofb - (int) (((float) yticv + *yadd) * *sca);
       XDrawLine(theDisp,win,theGC,ofl,ofb,s_0,iy);
       nintvl--;
   }
   iy1 = iy; ix1 = s_0;	/* remember position */
 }

/* Now put in the interum tic marks and labels. */
 vertadj = (f_height/2);
 s_1 = nintvl;

/* Initial label position to test against.   */
 last_label_pixel = xrt_height;

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
    XDrawLine(theDisp,win,theGC,ix1,iy1,s_0,iy);
    XDrawLine(theDisp,win,theGC,s_0,iy,s_4,iy);
    iy1 = iy; ix1 = s_4;	/* remember position */

    s_3 = iy + vertadj;            /* font centered vertically     */
    if (sid == 0) {                /* position label left or right */
      s_5 = ofl - (((label_width+1) * f_width));
    } else {
      s_5 = ofl + f_width +4;      /* leave a bit of space between number and tic */
    }

    if (s_3 < (last_label_pixel - (f_height + vertadj))) {
      n = (int) strlen(sstr);
      XftDrawString8(draw, &xft_color,fst,s_5,s_3,(XftChar8 *) sstr,n);
      last_label_pixel = s_3;
      XDrawLine(theDisp,win,theGC,ix1,iy1,s_2,iy);	/* extra tic length at label */
    }
    ix = ix1 = ofl; iy1 = iy;	/* remember position */
    yticv += ddy;
  }

/* Finish off the rest of the axis if less than the full width  */
  ix = ofl;
  iy = ofb - (int) ((*ymx + *yadd) * *sca);
  XDrawLine(theDisp,win,theGC,ix1,iy1,ix,iy);

/*
 Print out the axis label on left or right.  Loop through each
 character in the string and placing in a buffer for printing.
 If label on right ensure a bit of space between characters and
 the right edge of box to allow for image capture. If on right
 offset by 4 characters.
*/
  if (sid == 0) {
      ix = dbx1.b_left + (2 *f_width);
  } else {
      ix = dbx1.b_right - (4 * f_width);
  }
  mid = oft + ((ofb - oft)/2);
  iy = mid - (vertadj * ilen);
  if ((ofb - oft) > (f_height * ilen)){
    for (l = 0; l < ilen; ++l) {
      buf[0] = msg[l]; buf[1] = '\0';
      XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) buf,1);
      iy = iy + f_height;
    }
  }
  if (saved_font != butn_fnt) winfnt_(&saved_font);
  XftDrawDestroy(draw);
  return;
} /* vrtaxsdd_ */


/* ************** HORAXSddwwc *********************** */
/*
 Write horizontal axis attributes to file.
*/

void horaxisddwwc_(float *xmn,float *xmx,long int *offl,long int *offr,long int *offb,
        float *xadd,float *sca,long int *mode,float *dddx, long int *nnx, char* msg,int mlen)
{
 int ilen;
 char msg2[80];

 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';
 if ( wwc_ok == 1) {
   fprintf(wwc,"*horaxis\n");
   fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode);
   fprintf(wwc,"%s\n",msg2);
 }
 return;
} /* horaxisddwwc_ */


/* ************** HORAXSdd *********************** */
/*
 Construct and draw a horizontal axis where: XMN,XMX are the data
 minimum & maximum values, offL & offB are the pixel coords of the
 left start of the axis.  SCA is the scaling factor and Xadd is a data
 offset to adjust plotting for various data ranges. mode defines how
 left starting point is adjusted. ddx is data interval, nnx number
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
 XftDraw *draw;
 int s_1, s_2, s_3, s_4, s_5;
 int ofl,ofb,ofr;
 char sstr[10];
 int l, n, ix, iy, ix1, iy1, nintvl, ilen;
 int last_label_right_pixel, label_width, mid;
 long int nx,wticc,mde,saved_font;
 float ddx,xticv,rintvl,resid;
 char msg2[80];

 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';
 saved_font = current_font;
 if (saved_font != butn_fnt) winfnt_(&butn_fnt);

 if ( wwc_ok == 1) {
   fprintf(wwc,"*horaxis\n");
   fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode);
   fprintf(wwc,"%s\n",msg2);
 }

 ofl = (int) *offl; ofr = (int) *offr; ofb = (int) *offb; mde = *mode;
 nx = *nnx; ddx = *dddx;

/* Find the maximum label text width.  */
 label_width = 0;
 xticv = *xmn;
 labelstr(&nx, &xticv, &wticc, sstr);
 label_width = (int) strlen(sstr);
 xticv = *xmx;
 labelstr(&nx, &xticv, &wticc, sstr);
 if( (int) strlen(sstr) > label_width) label_width = (int) strlen(sstr);

// Define local drawable for Xft font.
 draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* Draw a vertical axis. */
 ix = ix1 = ofl;
 iy = iy1 = ofb;

/*
 If mode=1 and axis does not begin on an integer the location of
 the first tic needs to be adjusted.
*/
 xticv = *xmn;
 rintvl = (*xmx - *xmn) / ddx + 1.0;
 nintvl = (int)rintvl;
 if (mde == 1) {
   resid = *xmn - (int) *xmn;
   if(*xmn < 0. && fabs(resid) > 0.0001) {  /* ?? fabs((double)resid) */
       xticv = *xmn;
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       XDrawLine(theDisp,win,theGC,ofl,ofb,ix,iy);
   } else if(*xmn > 0. && fabs(resid) > 0.0001) {  /* ?? fabs((double)resid) */
       xticv = (*xmn + ddx);
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       XDrawLine(theDisp,win,theGC,ofl,ofb,ix,iy);
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
   XDrawLine(theDisp,win,theGC,ix1,iy1,ix,iy);
   s_4 = iy + 5;                    /* tic descender position  */
   XDrawLine(theDisp,win,theGC,ix,iy,ix,s_4);
   s_2 = ix - wticc;                /* current label position  */
   s_5 = ix - ((label_width * f_width)/2); /* position if all label characters */
   s_3 = iy +f_height + 5; /* bottom of font  */
   n = (int) strlen(sstr);
   if (s_5 >= (last_label_right_pixel + f_width)) {
     XftDrawString8(draw, &xft_color,fst,s_2,s_3,(XftChar8 *) sstr,n);
     last_label_right_pixel = s_5 + (label_width * f_width);
     XDrawLine(theDisp,win,theGC,ix,iy,ix,s_4+2);  /* extra tic length at label */
   }
   ix = ix1 = ofl; iy1 = iy;	/* remember position */
   xticv += ddx;
 }
/* Finish off the rest of the axis if less than the full width  */
 ix = ofl + (int) ((*xmx + *xadd) * *sca);
 iy = ofb;
 XDrawLine(theDisp,win,theGC,ix1,iy1,ix,iy);

/* Print out the axis label. */
 iy = dbx1.b_bottom-10;
 mid = ofl + ((ofr - ofl)/2);
 ix = mid - (f_width * ilen /2);
 if (ix > 5){
   XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) msg2,ilen);
 }
 if (saved_font != butn_fnt) winfnt_(&saved_font);
 XFlush(theDisp);
 XftDrawDestroy(draw);
 return;
} /* horaxisdd_ */


/* ************** HORAXSHDWDD *********************** */
/*
 Construct and draw a hour/day/week horizontal axis where: XMN,XMX are the data
 minimum & maximum values, offL & offB are the pixel coords of the
 left start of the axis.  SCA is the scaling factor and Xadd is a data
 offset to adjust plotting for various data ranges. mode defines how
 left starting point is adjusted. ddx is data interval, nx number
 of decimal places to use.
 ind = 0 display & tics per timestep, 1 display & tics per hour,
       2 display & tics per day, 3 display & tics per week,
       4 display & tics day-of-the-week
 idiv is the divisor for data values to get tic character e.g. when
      ind is zero idiv is one, when ind is one idiv is the number of
      timesteps per hour etc.
 isjday is the julian day of the year at the left of the axis in the
      case of ind = 2 or 4 (otherwise ignored)
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
 XftDraw *draw;
 int s_1, s_2, s_3, s_4, s_5;
 int ofl,ofb,ofr;
 char sstr[10];
 int l, n, ix, iy, ix1, iy1, nintvl, ilen,iind,iidiv,iisjday;
 int last_label_right_pixel, label_width, mid;
 long int nx,wticc,mde,saved_font;
/* xticv is actual timestep value, xxticv is for converted tic */
 float xticv,xxticv,ddx,rintvl,resid;
 char msg2[80];

 f_to_c_l(msg,&mlen,&ilen); strncpy(msg2,msg,(unsigned int)ilen); msg2[ilen] = '\0';
 saved_font = current_font;
 if (saved_font != butn_fnt) winfnt_(&butn_fnt);

 if ( wwc_ok == 1) {
   fprintf(wwc,"*horaxishdw\n");
   fprintf(wwc,"%f %f %ld %ld %ld %f %f %ld\n",
                *xmn,*xmx,*offl,*offr,*offb,*xadd,*sca,*mode);
   fprintf(wwc,"%s\n",msg2);
 }

 ofl = (int) *offl; ofr = (int) *offr; ofb = (int) *offb; mde = *mode;
 nx = (int) *nnx; ddx = *dddx;
 iind = (int) *ind; iidiv = (int) *idiv; iisjday = (int) *isjday;

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

// Define local drawable for Xft font.
 draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* Draw a vertical axis. */
 ix = ix1 = ofl;
 iy = iy1 = ofb;

/*
 If mode=1 and axis does not begin on an integer the location of
 the first tic needs to be adjusted.
*/
 xticv = *xmn;
 rintvl = (*xmx - *xmn) / ddx + 1.0;
 nintvl = (int)rintvl;
 if (mde == 1) {
   resid = *xmn - (int) *xmn;
   if(*xmn < 0. && fabs(resid) > 0.0001) {  /* ?? fabs((double)resid) */
       xticv = *xmn;
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       XDrawLine(theDisp,win,theGC,ofl,ofb,ix,iy);
   } else if(*xmn > 0. && fabs(resid) > 0.0001) {  /* ?? fabs((double)resid) */
       xticv = (*xmn + ddx);
       ix = ofl + (int) (((float) xticv + *xadd) * *sca);
       iy = ofb;
       XDrawLine(theDisp,win,theGC,ofl,ofb,ix,iy);
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
   XDrawLine(theDisp,win,theGC,ix1,iy1,ix,iy);
   s_4 = iy + 5;                    /* tic descender position  */
   XDrawLine(theDisp,win,theGC,ix,iy,ix,s_4);
   s_2 = ix - wticc;                /* current label position  */
   s_5 = ix - ((label_width * f_width)/2); /* position if all label characters */
   s_3 = iy +f_height + 5; /* bottom of font  */
   n = (int) strlen(sstr);
   if (s_5 >= (last_label_right_pixel + f_width)) {
     XftDrawString8(draw, &xft_color,fst,s_2,s_3,(XftChar8 *) sstr,n);
     last_label_right_pixel = s_5 + (label_width * f_width);
     XDrawLine(theDisp,win,theGC,ix,iy,ix,s_4+2);  /* extra tic length at label */
   }
   ix = ix1 = ofl; iy1 = iy;	/* remember position */
   xticv += ddx;
 }
/* Finish off the rest of the axis if less than the full width  */
 ix = ofl + (int) ((*xmx + *xadd) * *sca);
 iy = ofb;
 XDrawLine(theDisp,win,theGC,ix1,iy1,ix,iy);

/* Print out the axis label. */
 iy = dbx1.b_bottom-10;
 mid = ofl + ((ofr - ofl)/2);
 ix = mid - (f_width * ilen /2);
 if (ix > 5){
   XftDrawString8(draw, &xft_color,fst,ix,iy,(XftChar8 *) msg2,ilen);
 }
 if (saved_font != butn_fnt) winfnt_(&saved_font);
 XFlush(theDisp);
 XftDrawDestroy(draw);
 return;
} /* horaxishdwdd_ */

/* *************** ESRU menu text update. *************** */
/*
 This function takes an array of strings from f77 and stores
 it in the static array m_list for subsequent use by other functions.
*/
/* char m_list[MENU_LIST_LEN][125]; character arrays for menu buffer */
/* char mtype_list[MENU_LIST_LEN]; character array representing m_list array use */
/* int m_width = 0; current menu max line length */
/* int m_lines = 0; current number of active menu lines */
void updmenu_(items,itypes,nitmsptr,iw,len_items)
  char      *items;         /* f77 array of menu text strings    */
  char      *itypes;        /* f77 character array (nitmsptr wide)    */
  long int  *nitmsptr;      /* number of menu lines to display   */
  long int  *iw;            /* actual max char width in items    */
  int  len_items;           /* length of menu string from f77    */
{
  int	i, j, k;
  int	m_line = *nitmsptr;

  m_width = *iw;	/* remember width of menu text */
  m_lines = m_line;	/* remember number of lines */
  if(m_lines == 0)return;	/* don't bother if no lines */
  strncpy(mtype_list,itypes,(unsigned int)m_lines);	/* copy to static array */

  k = 0;
  for(i = 0; i < m_lines; i++) {	/* for each line...  */
    for(j = 0; j < len_items; j++) {	/* for each character...  */
      m_list[i][j] = items[k];
      k = k +1;   /* increment for next char in items (a fortran string array does not have
                     nulls between strings in array, it just looks like one long string) */
    }
    m_list[i][len_items] = '\0';	/* write terminator  */
/* debug  fprintf(stderr,"m_list %s %d %d %d %d\n",m_list[i],i,k,m_width,len_items); */
  }
  return;
}

/* ******  Move mouse help button on screen ********** */
/* Box covered by menu -> move to the left. */
void movemse()
/* local variables */
{
  XftDraw *draw;
  long int saved_font;
  int bh;

  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);
  bh = f_height+2;	/* box height is font height +2 */
  mouse.b_top = dbx1.b_bottom - bh - bh;    /* double height box here */
  mouse.b_bottom = dbx1.b_bottom - 2;
  mouse.b_left = mouse.b_right - (f_width * 8) +4;
  xbox(mouse,fg,white, BMCLEAR | BMEDGES);
  mouse_avail = 1;             /* tell the world that mouse help is available */
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);
  XftDrawString8(draw, &xft_color,fst,mouse.b_left+2,mouse.b_bottom-2," mouse ",7);
  XftDrawDestroy(draw);

/* Draw boxes representing buttons */
  mouse1.b_top = mouse.b_top + 1;
  mouse1.b_bottom = mouse.b_top + bh;
  mouse1.b_left = mouse.b_left + 2;
  mouse1.b_right = mouse1.b_left + bh;
  xbox(mouse1,fg,white, BMCLEAR | BMEDGES);
  mouse2.b_top = mouse.b_top + 1;
  mouse2.b_bottom = mouse.b_top + bh;
  mouse2.b_left = mouse1.b_right + 2;
  mouse2.b_right = mouse2.b_left + bh;
  xbox(mouse2,fg,white, BMCLEAR | BMEDGES);
  mouse3.b_top = mouse.b_top + 1;
  mouse3.b_bottom = mouse.b_top + bh;
  mouse3.b_right = mouse.b_right - 2;
  mouse3.b_left = mouse3.b_right - bh;
  xbox(mouse3,fg,white, BMCLEAR | BMEDGES);
  if (saved_font != butn_fnt) winfnt_(&saved_font);  /* restore std font */

  return;
} /* movemse */

/* ******  Remove mouse help button on screen ********** */
void closemouse_()
{
  mouse_avail = 0;             /* tell the world that mouse help is not available */
  mouse.b_bottom = mouse.b_bottom + 1;
  mouse.b_right = mouse.b_right + 1;
  xbox(mouse,fg,white, BMCLEARALL);

  return;
} /* closemouse */

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
  XEvent event;
  XWindowAttributes wa;
  Pixmap under;		        /* to save image under help box  */
  KeySym     ks;
  XftDraw *draw;
  static char buf[80];
  static int blen = 0;
  char	keypressed,menuentry;
  long int saved_font, use_font, changed_font, label_font;
  int bottom,left;   /* ll of capture box */
  int   mob_height, mob_width,mib_height,mib_width,xb,yb,x,y,i,iy,pflg,lt1,iw,index,lineheight;
  int   u_height, u_width;      /* size of the under pixmap */
  int   menu_height;   /* pixel height for text of menu */
  box	menubx, gmenubx;	/* menu display, outer box */
  box   hl_box;       /* for inverting box around text */
  Bool exp = 1;
  int	no_valid_event,config_altered,iaux,butid;
  int   oldi,notted;    /* remember last motion hilight and if an item yet hilighted */
  unsigned int start_height,start_width;

// Offsets from right are defined via character widths which may be
// inaccurate for proportional fonts.
  elevplus_left = disp.b_right - (f_width * 31);
  elevminus_left = disp.b_right - (f_width * 34);
  elev_left = disp.b_right - (f_width * 46);
  aziplus_left = disp.b_right - (f_width * 50);
  aziminus_left = disp.b_right - (f_width * 53);
  azi_left = disp.b_right - ((f_width * 63)+4);

  if(m_lines == 0)return;	/* don't bother if no menu */
  iw = (int) *iwth;  /* character width to display */

  xb = (int) *impx;  yb = (int) *impy; pflg = (int) *ipflg;
  f_to_c_l(titleptr,&len_title,&lt1);  /* get actual width of title xrt_height*/
  changed_font = 0; label_font = 0;
  saved_font = use_font = current_font; /* save existing font  */
  *irpx = (long int) 0;
  *irpy = (long int) 0;

/* remember position and size of the whole module (so as to detect changes) */
  XGetWindowAttributes(theDisp,win,&wa);
  start_height = (unsigned int)wa.height; start_width = (unsigned int)wa.width; config_altered = 0;
/* debug  fprintf(stderr,"evwmenu: sh sw sx sy %d %d \n",start_height,start_width); */

/* Begin by changing to the current menu font. */
  if(saved_font != menu_fnt) { winfnt_(&menu_fnt); use_font = menu_fnt; }

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/*  Check if the text will fit within window, if not down-size until it does. */
  menu_height = (m_lines+1)*(f_height+2);
  while (xrt_height < menu_height) {
    if (use_font == 0  || use_font == 4 ) break;
    use_font-- ;
    winfnt_(&use_font);
    menu_height = (m_lines+1)*(f_height+2);
    changed_font = 1;
  }

  /* having established a font, figure out heights and widths of outer and inner box */
  mib_height = (m_lines+1)*(f_height+2);   /* inner pixel height */
  mob_height = mib_height + (5 + (f_height + 8));  /* include space for title  */
  mib_width = (iw*f_width+2);	/* inner pixel width */
  mob_width = mib_width +20;	/* box slightly wider than longest line */
/*
 If a specific box starting point has been passed then try to honor
 this otherwise place it slightly above the dialogue box.
*/
  if (xb == 0) {
    xb= xrt_width - (mob_width + 2);  yb = mob_height + 2;
  }

/* Create the menu box. */
  gmenubx.b_top   = yb - mob_height;	/* top border with space for title */
  gmenubx.b_bottom= yb;	                /* bottom border with space for dismiss */
  gmenubx.b_left  = xb;  gmenubx.b_right = xb + mob_width;
  menubx.b_top   = gmenubx.b_top + (f_height + 8);
  menubx.b_bottom= gmenubx.b_bottom - 5;
  menubx.b_left  = gmenubx.b_left + 5;  menubx.b_right = gmenubx.b_right - 5;
  hl_box.b_left = menubx.b_left +2;  hl_box.b_right = menubx.b_right -2;
  lineheight = f_height+2;

/* Check to see if menu is over mouse help button */
  if (mouse_avail > 0) {
    if (gmenubx.b_bottom>mouse.b_top&&gmenubx.b_left<mouse.b_right) {
      closemouse_();
      mouse.b_right = gmenubx.b_left - 4;
      movemse();
    }
  }

  u_width = WIDTH(gmenubx); u_height = HEIGHT(gmenubx); /* width & height area to save */
  under = XCreatePixmap(theDisp,win,(unsigned int) u_width,(unsigned int) u_height,dispDEEP);
  box_to_pix(win,gmenubx,under,u_width,u_height);   /* save rect under gmenubx to under */

  xbox(gmenubx,fg,gpopfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
  xbox(gmenubx,fg,bg,BMEDGES);
  xbox(menubx,fg,white,BMCLEAR|BMEDGES);	/* draw menu display box  */

  XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,menubx.b_top-5,(XftChar8 *) titleptr,lt1);

/* display lines of text.  */
  for ( i = 0; i < m_lines; i++ ) {
    iy = menubx.b_top + ((i + 1) * (f_height+2)) + 5;
    XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) m_list[i],iw);
  }
  XFlush(theDisp); /* force drawing of menu text */

  oldi = OFFEND;
  notted = FALSE;
  no_valid_event = TRUE;
  while ( no_valid_event) {
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case Expose:
/* debug  fprintf(stderr,"evwmenu: exp event x %d y %d w %d h %d \n",event.xexpose.x,
            event.xexpose.y,event.xexpose.width,event.xexpose.height); */
   	break;
      case VisibilityNotify:
        if(event.xvisibility.state == 0 ) {
          no_valid_event = FALSE;
          iaux = aux_menu((XEvent *) &event);
          i = OFFEND;
        }
   	break;
      case ConfigureNotify:

/* This returns the absolute upper left pixel position on the monitor of the application. */
/* debug fprintf(stderr,"evwmenu: ev sx sy %d %d \n",event.xconfigure.x,event.xconfigure.y);  */

        XGetWindowAttributes(theDisp,win,&wa);
/* debug  fprintf(stderr,"evwmenu: xwa sh sw sx sy %d %d %d %d \n",wa.height,wa.width,wa.x,wa.y); */
        if(start_height == (unsigned int)wa.height && start_width == (unsigned int)wa.width) {	/* no need to update window */
          no_valid_event = TRUE;
        }
        if(start_height != (unsigned int)wa.height || start_width != (unsigned int)wa.width) {	/* window resized so force update */
          config_altered = 1;
          no_valid_event = FALSE;
          iaux = aux_menu((XEvent *) &event);
          i = OFFEND;
        }
        no_valid_event = FALSE;
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;  butid = (int)event.xbutton.button;
        if (xboxinside(menubx,x,y)){

/* user has clicked within the bounds of a item, confirm by hilighting item */
           i = (( y - (menubx.b_top + 6)) / lineheight);
           if ( i < m_lines ) {	/* i.e. one of the items */
             no_valid_event = FALSE;
             iy = menubx.b_top + ((i + 1) * (f_height+2)) + 5;
             hl_box.b_bottom = iy + 3;
             hl_box.b_top = hl_box.b_bottom - (f_height+4);
/* debug     fprintf(stderr,"m_list %s %d %d %d %d\n",m_list[i],i,iy,hl_box.b_bottom,hl_box.b_top); */
             xbox(hl_box,fg,ginvert, BMCLEAR | BMNOT );        /* grey item */
             XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
             XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) m_list[i],iw);
             XFlush(theDisp);
             XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
             Timer(300);
             xbox(menubx,fg,white,BMEDGES|BMCLEAR);	/* clear menu box */
             *ino = (long int) i + (long int) 1;  /* compensate for c starting at zero */
             *uresp = (long int) 0;
/* debug     fprintf(stderr,"click %s %d %d\n",m_list[i],i,*ino); */
   	     break;
           }
         } else if (!xboxinside(gmenubx,x,y)){   /* outside menu - check aux menu */
           no_valid_event = FALSE;
/* debug   fprintf(stderr,"outside menu at x %d y %d\n",x,y); */
           *irpx=(long int) x;
           *irpy=(long int) y;
           iaux = aux_menu((XEvent *) &event);
           i = OFFEND;
           if ( iaux == 5 ) {
/* debug     fprintf(stderr,"Inside graphics display x %d y %d button %d\n",x,y,butid); */
             *uresp=(long int) butid;    /* Set to id of button pressed */
           }
           break;
         }
      case MotionNotify:	/* (XKeyEvent)&ev  */
        x = event.xmotion.x;  y = event.xmotion.y;
        if (xboxinside(menubx,x,y)) {
          i = (( y - (menubx.b_top + 6)) / lineheight);
          if ( i < m_lines ) {	/* one of the items */
            if ( oldi != i ) {	/* changed area */
              if ( notted ) {	/* clear and reprint text of previous hilighted item */
                iy = menubx.b_top + ((oldi + 1) * (f_height+2)) + 5;
                hl_box.b_bottom = iy + 3;
                hl_box.b_top = hl_box.b_bottom - (f_height+4);
                xbox(hl_box,fg,white, BMCLEAR | BMNOT);
                XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) m_list[oldi],iw);
                XNextEvent (theDisp,&event);	/* flush  */
              }
              iy = menubx.b_top + ((i + 1) * (f_height+2)) + 5;  /* hilight the current item */
              hl_box.b_bottom = iy + 3;
              hl_box.b_top = hl_box.b_bottom - (f_height+4);
/* debug      fprintf(stderr,"m_list %s %d %d %d %d\n",m_list[i],i,iy,hl_box.b_bottom,hl_box.b_top);  */
              xbox(hl_box,fg,gmenuhl, BMCLEAR | BMNOT);
              XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) m_list[i],iw);
              XNextEvent (theDisp,&event);	/* flush  */
              oldi = i;   /* remember item hilighted and that one is hilighted*/
              notted = TRUE;
            }
          }
        } else if ( notted ) {	/* check for highlighted item and unhighlight if found */
          iy = menubx.b_top + ((oldi + 1) * (f_height+2)) + 5;
          hl_box.b_bottom = iy + 3;
          hl_box.b_top = hl_box.b_bottom - (f_height+4);
          xbox(hl_box,fg,white, BMCLEAR | BMNOT);
          XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) m_list[oldi],iw);
          XNextEvent (theDisp,&event);	/* flush  */
          oldi = OFFEND;
          notted = FALSE;
        }
        break;
      case KeyPress:	/* (XKeyEvent)&ev */
        blen = XLookupString((XKeyEvent*)&event,buf,80,&ks,(XComposeStatus *) NULL);
        if(blen > 0) {
          if (buf[0] == '\r' || buf[0] == '\n' ) {
            no_valid_event = FALSE;
            *uresp = (long int) 0;
            xbox(menubx,fg,white,BMEDGES|BMCLEAR);	/* clear help box */
   	    break;
          } else if (buf[0] == '\010') {  /*  buf[0] is BS  */
            XBell(theDisp, 50);  /*  buf[0] is BS but at left edge  */
   	    break;
          } else if (buf[0] == '\177') {  /*  buf[0] is DEL  */
            XBell(theDisp, 50);  /*  buf[0] is BS but at left edge  */
   	    break;
          } else if (buf[0] == '\011') {
            XBell(theDisp, 50);
   	    break;
          } else if (buf[0] == '\033') {
            XBell(theDisp, 50);
   	    break;
          } else if (buf[0] >= ' ' && buf[0] < '\177') {
/* debug    fprintf(stderr,"buf[0] is key %c \n",buf[0]); */
            for ( index = 0; index < m_lines; index++ ) {
              keypressed = isupper (buf[0]) ?
		tolower(buf[0]) : buf[0];
              menuentry = isupper (m_list[index][0]) ?
		tolower (m_list[index][0]) : m_list[index][0];
              if ( keypressed == menuentry ) {

/* user has pressed a key equivalent to an item first character, confirm by hilighting item */
                no_valid_event = FALSE;
                *ino = (long int) index + (long int) 1;   /* add 1 to compensate for c starting at zero */
                *uresp = (long int) 0;
/* debug        fprintf(stderr,"key %s %d %d\n",m_list[index],index,*ino); */
                iy = menubx.b_top + ((index + 1) * (f_height+2)) + 5;
                hl_box.b_bottom = iy + 3;
                hl_box.b_top = hl_box.b_bottom - (f_height+4);
/* debug        fprintf(stderr,"m_list %s %d %d\n",m_list[index],index,iy); */
                xbox(hl_box,fg,ginvert, BMCLEAR | BMNOT );        /* invert box */
                XSetForeground(theDisp,theGC, white); XSetBackground(theDisp,theGC, ginvert);
                XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) m_list[index],iw);
                XFlush(theDisp);
                XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
                Timer(300);
                xbox(menubx,fg,white,BMEDGES|BMCLEAR);	/* clear menu */
   	        break;
              }
            }
   	    break;
          }
        } else {
          if (azi_avail >=1) {
            if (ks==XK_Left || ks==XK_KP_Left) { /* left arrow pressed */
              no_valid_event = FALSE;
              saved_font = current_font; bottom = disp.b_top; left = aziminus_left;
              dosymbox(aziminus,2,&saved_font,&box_fnt,&bottom,&left,"aziminus",'!');
            break;
            } else if (ks==XK_Right || ks==XK_KP_Right) { /* right arrow pressed */
              no_valid_event = FALSE;
              saved_font = current_font; bottom = disp.b_top; left = aziplus_left;
              dosymbox(aziplus,2,&saved_font,&box_fnt,&bottom,&left,"aziplus",'!');
            break;
            } else if (ks==XK_Up || ks==XK_KP_Up) { /* up arrow pressed */
              no_valid_event = FALSE;
              saved_font = current_font; bottom = disp.b_top; left = elevplus_left;
              dosymbox(elevplus,2,&saved_font,&box_fnt,&bottom,&left,"elevplus",'!');
            break;
            } else if (ks==XK_Down || ks==XK_KP_Down) { /* down arrow pressed */
              no_valid_event = FALSE;
              saved_font = current_font; bottom = disp.b_top; left = elevminus_left;
              dosymbox(elevminus,2,&saved_font,&box_fnt,&bottom,&left,"elevminus",'!');
            break;
            }
          }
        }
        break;
    }
  }

  XClearArea(theDisp,win,gmenubx.b_left,gmenubx.b_top,(unsigned int) u_width,(unsigned int) u_height,exp);
  if(config_altered == 0)pix_to_box(under,u_width,u_height,gmenubx,win);
  XFreePixmap(theDisp, under);
  XftDrawDestroy(draw);
  if(XPending(theDisp) > 0) {
/* debug  fprintf(stderr,"evwmenu: events remaining %d\n",XPending(theDisp)); */
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  if (changed_font == 1) winfnt_(&saved_font);  /* Restore font.  */
  XFlush(theDisp); /* added to force draw */
  if(config_altered == 1) refreshenv_();
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
  char *test = "_A_V_";
  char *testsp = "     ";
  char *testul = "_____";
  KeySym     ks;
  XGlyphInfo info;
  long int saved_font, use_font, changed_font, label_font;
  int   mib_width,i,iy,lt1,iw;
  int   lt,ltsp,ltul;  /* lengths of test strings */
  int   vfw,vfwsp,vfwul; /* avarage font widths */
  int fonth,imw,foundul;
  int pixwthll,pixwthbl,pixwthul;
  
  if(m_lines == 0)return;	/* don't bother if no menu */
  iw = (int) *iwth;  /* character width to display */
  pixwthll=0,pixwthbl=0,pixwthul=0;     /* initially zero */ 
  changed_font = 0; label_font = 0;
  saved_font = use_font = current_font; /* save existing font  */

/* Begin by changing to the current menu font. */
  if(saved_font != menu_fnt) { winfnt_(&menu_fnt); use_font = menu_fnt; }

  mib_width = (iw*f_width+2);	/* inner pixel width */

// Get width of the title
  lt1=clnblnk(titleptr);
  XftTextExtents8(theDisp,fst,titleptr,lt1,&info);
  fonth = fst->ascent + fst->descent;
// debug fprintf(stderr,"fst XftTextExtents8 %ld %d %d %d %d avgfw %d\n",strlen(titleptr),lt1,fonth,info.width,info.xOff,info.xOff/lt1);
//  fprintf(stderr,"of %s\n",titleptr);

/* for each of the menu items... Find a way to exlude lines with lots of _ */
  for ( i = 0; i < m_lines; i++ ) {
    foundul=0;
    if (strstr(m_list[i],"______")) foundul = 1;
    if (foundul==1){
    } else {
      imw=clnblnk(m_list[i]);
      XftTextExtents8(theDisp,fst, m_list[i],imw,&info);
      fonth = fst->ascent + fst->descent;
//      fprintf(stderr,"menu XftTextExtents8 %d %d %d %d %d %d %d %d avgfw %d\n",iw,imw,fonth,info.width,info.height,info.x,info.y,info.xOff,info.xOff/imw);
//      fprintf(stderr,"of %s\n",m_list[i]);
      if(info.xOff > pixwthll) pixwthll = info.xOff;
    }
  }
//  fprintf(stderr,"pixels for widest line %d\n",pixwthll);
//  fprintf(stderr,"menu pixels allocated %d\n",mib_width);

/* Get average width of test strings. */
  lt=clnblnk(test);
  XftTextExtents8(theDisp,fst,test,lt,&info);
  vfw = (info.xOff/lt);
  ltsp=clnblnk(testsp);
  XftTextExtents8(theDisp,fst,testsp,ltsp,&info);
  vfwsp = (info.xOff/ltsp);
  ltul=clnblnk(testul);
  XftTextExtents8(theDisp,fst,testul,ltul,&info);
  vfwul = (info.xOff/ltul);
  fprintf(stderr,"average font width %d spaces %d _ %d\n",vfw,vfwsp,vfwul);

  if (changed_font == 1) winfnt_(&saved_font);  /* Restore font.  */
  *ipixwthma=(long int)mib_width;  /* allocated width */
  *ipixwthll=(long int)pixwthll;   /* pixels for longest line */
  *ivfw=(long int)vfw;             /* average test characters pixel width */
  *ivfwsp=(long int)vfwsp;         /* average for spaces pixel width */
  *ivfwul=(long int)vfwul;         /* average for _ pixel width */
  return;
}

/* ****************** auxulliary menu
  Used to test for mouse click in other portions of the screen.
*/
int aux_menu(event)  XEvent *event; {

  XftDraw *draw;
  int x,y,k,x_old,y_old,win_x,win_y,idiff,butid;     /* current cursor postion and pressed button id */
  int vert,no_valid_event;
  long int saved_font;
  char avail_cfg;	/* current char of config button to pass to fortran. */
  int len_avail = 1;    /* length of character passed back */
  long int eyex,eyey;  /* centre for image symbols and symbol index and size */
  long int sym,sz;      /* symbol and symbol size      */
  int but_rlse = 0;
  int bottom,left;	/* ll of capture box */
  int bh;
  int scrlold,scrlast;		/* old position of scroll bar */
  box msehbx;

  int iline=0;
  long int stype=0;
  int pixs=16;
  int choice;  /* graphic pop up menu returned choice */

  long int iugx,iugy;  /* graphic pop up menu position */
  long int is,isa; /* IS = index of the icon, ISA = index of the connection waypoint. */

/* graphics update options */
  long int zoom=0;
  long int grid_rz=0;
  long int grid_oo=0;
  long int snap=0;
  long int ipan=0;
  long int tfm=0;
  long int nselect2=0;
  long int nselect3=0;
  long int active=0;
  float dx=0.0;
  float dy=0.0;
  long int idx=0;
  long int idy=0;
  long int ifrlk=0;

/* update left position of boxes along horizontal bar */
   saved_font = menu_fnt;
   if (saved_font != butn_fnt) winfnt_(&butn_fnt);
   wire_left = disp.b_right - (f_width * 28);
   capture_left = disp.b_right - (f_width * 10);
   elevplus_left = disp.b_right - (f_width * 31);
   elevminus_left = disp.b_right - (f_width * 34);
   elev_left = disp.b_right - (f_width * 46);
   aziplus_left = disp.b_right - (f_width * 50);
   aziminus_left = disp.b_right - (f_width * 53);
   azi_left = disp.b_right - ((f_width * 63)+4);
   if (saved_font != butn_fnt) winfnt_(&saved_font);  /* restore std font */

// Define local drawable for Xft font.
   draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* debug  fprintf(stderr,"aux_menu event type %d\n",event->type);  */
  switch (event->type) {
    case VisibilityNotify:
      if(event->xvisibility.state == 0 ) {
        refreshenv_();
      }
      break;
    case ConfigureNotify:
      refreshenv_();
      break;
    case ButtonPress:
      x = event->xbutton.x;  y = event->xbutton.y;  butid = (int)event->xbutton.button;
      if (dbx1_avail == 1 && xboxinside(dbx1,x,y)) {

/* The following code is used only if network graphics is on. */
/* Check if button press inside the graphics area. */
        if(network_gpc){

/* Add in a pointer motion mask so that mouse movement is regarded as an event */
         XSelectInput(theDisp,win, ExposureMask | ButtonPressMask | ButtonReleaseMask | KeyPressMask | PointerMotionMask | ButtonMotionMask |StructureNotifyMask | VisibilityChangeMask);

/* Case of first mouse click (with shift) return x-y to the fortran side */
          if(butid==1){

/* fprintf(stderr," inside graphic display at: xpos %d ypos %d button %ld \n",x,y,butid);*/
            x = event->xbutton.x;  y = event->xbutton.y;
            iugx=x; iugy=y;
            x_old=iugx; y_old=iugy;
            is=0;
            stype=0;
            nwkslctc_(&iugx,&iugy,&stype,&is,&isa,&nselect,&active);
            if(is>0&&nconnect){
              gconad_(&iugx,&iugy,&win_x,&win_y,&butid);
              iugx=win_x;
              iugy=win_y;
            }

/* If a component has been selected and mouse is moving, draw a moving connection
line button 1 = start button2 = intermediate point button3 = end */
/* fprintf(stderr," tracking position: xpos %d ypos %d is %ld \n",x,y,is); */
            no_valid_event = TRUE;
            butid=0;
            if(is>0&&stype==1&&nconnect){
              iline=1;
              x_old=iugx;y_old=iugy;
              while (butid!=3&&butid!=1){
                XNextEvent(theDisp,event);   /* get next event */
                switch(event->type) {
                  case ButtonPress:	/* check what button was pressed and act accordingly */
                    butid = (int)event->xbutton.button;
                    if(butid==2){
                      iugx=event->xbutton.x;
                      iugy=event->xbutton.y;
                      gconad_(&iugx,&iugy,&win_x,&win_y,&butid);
                      x=win_x;
                      y=win_y;
                      XDrawLine(theDisp,win,theGC,iugx,iugy,x,y);
                      iugx=x;
                      iugy=y;
                    }
                    if(butid==3){
                      iugx=event->xbutton.x;
                      iugy=event->xbutton.y;
                      gconad_(&iugx,&iugy,&win_x,&win_y,&butid);
                      x=win_x;
                      y=win_y;
                      XDrawLine(theDisp,win,theGC,iugx,iugy,x,y);
                      iugx=x;
                      iugy=y;
                    }
                    break;
                } /* end switch */
                x=event->xmotion.x;
                y=event->xmotion.y;

/* draw the new line setting GXxor, re-drawn lines, will produce a copy of
the screen below linux uses GxorReversed or GXinvert with X11R6*/
#ifdef CYGW
                XSetFunction(theDisp,theGC,GXinvert); /*  X11R6 */
#endif
#ifdef LIN
                XSetFunction(theDisp,theGC,GXinvert); /*  X11R6 */
#endif
#ifdef OSX
                XSetFunction(theDisp,theGC,GXinvert); /*  X11R6 */
#endif
#ifdef WIN
   /* probably no need to do anything */
#endif
                XSetForeground(theDisp,theGC, fg);
                XDrawLine(theDisp,win,theGC,iugx,iugy,x_old,y_old);
                XFlush(theDisp);
                XDrawLine(theDisp,win,theGC,iugx,iugy,x,y);
                iline=0;
                x_old=x;
                y_old=y;

/*                default:
                  no_valid_event = FALSE;
                  break; */

              } /* end while */

/* Re-set the button id */
              butid=0;

/* Re-set GXxor or GXinvert to GXcopy */
              XSetFunction(theDisp,theGC,GXcopy);
              XSetForeground(theDisp,theGC,fg); XSetBackground(theDisp,theGC,bg);

/*refresh the grid display */
              gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
              nconnect=0;
              nselect=1;
              ndata=0;
            } /* end if */
/* reset connect option */
          }

          if(butid==3){
/*Check if the button has been clicked inside a component icon or a connection intermediate
point*/
            x = event->xbutton.x;  y = event->xbutton.y; butid = (int)event->xbutton.button;
            iugx=x; iugy=y;
            x_old=iugx;y_old=iugy;
            is=0;
            stype=0;
            nwkslctc_(&iugx,&iugy,&stype,&is,&isa,&nselect3,&active);
            if(is>0&&active>0){
              no_valid_event=TRUE;
              if(stype!=1){
                pixs=5;
              }else{
                pixs=16;
              }
/* Draw a square, move with the cursor until the button is released. */
              while(no_valid_event){
                XNextEvent(theDisp,event);
                switch(event->type){
                  case MotionNotify:
                    x=event->xmotion.x;
                    y=event->xmotion.y;
#ifdef CYGW
                    XSetFunction(theDisp,theGC,GXinvert); /*  X11R6 */
#endif
#ifdef LIN
                    XSetFunction(theDisp,theGC,GXinvert); /*  X11R6 */
#endif
#ifdef OSX
                    XSetFunction(theDisp,theGC,GXinvert); /*  X11R6 */
#endif
#ifdef WIN
   /* probably no need to do anything */
#endif
                    XSetForeground(theDisp,theGC,fg);
                    XDrawLine(theDisp,win,theGC,x_old-pixs,y_old-pixs,x_old+pixs,y_old-pixs);
                    XDrawLine(theDisp,win,theGC,x_old+pixs,y_old-pixs,x_old+pixs,y_old+pixs);
                    XDrawLine(theDisp,win,theGC,x_old+pixs,y_old+pixs,x_old-pixs,y_old+pixs);
                    XDrawLine(theDisp,win,theGC,x_old-pixs,y_old+pixs,x_old-pixs,y_old-pixs);
                    XFlush(theDisp);
                    XDrawLine(theDisp,win,theGC,x-pixs,y-pixs,x+pixs,y-pixs);
                    XDrawLine(theDisp,win,theGC,x+pixs,y-pixs,x+pixs,y+pixs);
                    XDrawLine(theDisp,win,theGC,x+pixs,y+pixs,x-pixs,y+pixs);
                    XDrawLine(theDisp,win,theGC,x-pixs,y+pixs,x-pixs,y-pixs);
                    x_old=x;
                    y_old=y;
                    butid = (int)event->xbutton.button;
                    no_valid_event=TRUE;
                  break;
                  case ButtonRelease:
                    no_valid_event=FALSE;
                    break;
                }
              }
/* Re-set GXxor to GXcopy */
              XSetFunction(theDisp,theGC,GXcopy);
              XSetForeground(theDisp,theGC,fg); XSetBackground(theDisp,theGC,bg);
              x = event->xbutton.x; y = event->xbutton.y;
              iugx=x; iugy=y;
              nwkupdtpos_(&is,&isa,&stype,&x,&y);
/* unselect the component */
              nwkslctc_(&iugx,&iugy,&stype,&is,&isa,&nselect3,&active);
            }
          }

          if(butid==2){   /* middle button on icon for flip rotate */
            iugx = x; iugy=y;
            is = 0;
            nwkslctc_(&iugx,&iugy,&stype,&is,&isa,&nselect2,&active);
            if(is>0&&stype==1&&active>0){
              epopup_("Transform",objgphcmenu,&iugx,&iugy,&choice);
              switch(choice){
                case 0:	/* rotate */
                  tfm=1; icntfm_(&tfm); break;
                case 1: /* flip vertical */
                  tfm=2; icntfm_(&tfm); break;
                case 2: /* flip horizontal */
                  tfm=3; icntfm_(&tfm); break;
                case 3:
                  break;
              }
            }else{
              fprintf(stderr," menu: display grid %ld snap %ld \n",grid_oo,snap);
              if(igrid>0&&isnap>0){
              epopup_("Display control",netgm2menuca,&iugx,&iugy,&choice);
              }else if(igrid>0&&isnap<1){
              epopup_("Display control",netgm2menucc,&iugx,&iugy,&choice);
              }else if(igrid<1&&isnap>0){
              epopup_("Display control",netgm2menucb,&iugx,&iugy,&choice);
              }else{
              epopup_("Display control",netgm2menucd,&iugx,&iugy,&choice);
              }
              switch(choice){
                case 0: /* zoom in */
                  zoom = 1;
                  gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                  zoom = 0;
                  break;
                case 1: /* zoom out */
                  zoom = -1;
                  gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                  zoom = 0;
                  break;
                case 2: /* grid spacing ++ */
                  grid_rz = 1;
                  gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                  grid_rz = 0;
                  break;
                case 3: /* grid spacing -- */
                  grid_rz = -1;
                  gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                  grid_rz = 0;
                  break;
                case 4: /* grid on/off */
                  if(igrid>0){
                    grid_oo=-1;
                    igrid=0;
                  }else{
                    grid_oo=1;
                    igrid=1;
                  }
                  gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                  grid_oo=0;
                  break;
                case 5: /* snap on/off */
                  if(isnap>0){
                    snap=-1;
                    isnap=0;
                  }else{
                    snap=1;
                    isnap=1;
                  }
                  gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                  snap=0;
                  break;
                case 6:
                  break;
              }
            }
          }
/* button click inside graphics area 3rd mouse button */

        } /* -> end of network graphics here */
/* debug  fprintf(stderr," inside graphic display at: xpos %d ypos %d \n",x,y); */
      }
      if (xboxinside(updown_text,x,y)) {
        vert = updown_text.b_left+15;
        no_valid_event = TRUE;
        while ( no_valid_event) {
          XNextEvent(theDisp,event);
          switch (event->type) {
            case MotionNotify:	/* while mouse is moving track position  */
              x = event->xmotion.x;  y = event->xmotion.y;
              idiff = (int)(updown_text.b_bottom-5) - (int)y;
              drawdwl(vert,updown_text.b_bottom-5,vert,y);
              XFlush(theDisp); /* added to force draw */
              XNextEvent(theDisp,event);
              if (event->type == ButtonRelease) {
                no_valid_event = FALSE;
                break;
              }
              break;
            case ButtonRelease:	/* button released so jump out of loop  */
              no_valid_event = FALSE;
              break;
            default:
              no_valid_event = FALSE;
              break;
          }
        }
        but_rlse = 2;    /* signal change in image as well as mouse  */
        disp.b_top = y;
        refreshenv_();
        return (but_rlse);
      } else if (xboxinside(scroll,x,y)) {
        if (xboxinside(scrollbar,x,y)) {
          vert = y;
          scrlold = scroll_index;
          no_valid_event = TRUE;
          while ( no_valid_event) {
            scrlast = scroll_index;
            XNextEvent(theDisp,event);
            switch (event->type) {
              case MotionNotify:	/* while mouse is moving track position  */
                y = event->xmotion.y;
                idiff = vert - y;
                scroll_index = scrlold - (int) ((idiff*edisp_index)/(disp.b_bottom - disp.b_top - 2));
                if ( scroll_index > edisp_index ) scroll_index = edisp_index;
                if ( scroll_index < tfb_limtty ) scroll_index = tfb_limtty;
                if (scroll_index != scrlast) disptext();
                break;
              case ButtonRelease:	/* button released so jump out of loop  */
                no_valid_event = FALSE;
                break;
              default:
                no_valid_event = TRUE;
                break;
            }
          }
        } else if (y > scrollbar.b_bottom) {		/* page up */
            scroll_index = scroll_index + tfb_limtty - 1;
            if ( scroll_index > edisp_index ) scroll_index = edisp_index;
            disptext();
        } else if (y < scrollbar.b_top) {		/* page down */
            scroll_index = scroll_index - tfb_limtty + 1;
            if ( scroll_index < tfb_limtty ) scroll_index = tfb_limtty;
            disptext();
        }
        but_rlse = 2;    /* signal change in image as well as mouse  */
        return (but_rlse);
      } else if (xboxinside(scrollv,x,y)) {
        if (xboxinside(scrollbarv,x,y)) {
          vert=y;
          no_valid_event = TRUE;
          while ( no_valid_event) {
            XNextEvent(theDisp,event);
            switch (event->type) {
              case MotionNotify:	/* while mouse is moving track position  */
                y = event->xmotion.y;
                ipan=2;
                dy=((float)(vert-y)/(float)(scrollbarv.b_bottom-scrollbarv.b_top));
                gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                dy=0.0;
                ipan=0;
                vert=event->xmotion.y;
/* update the network on the screen */
                break;
              case ButtonRelease:	/* button released so jump out of loop  */
                no_valid_event = FALSE;
                break;
              default:
                no_valid_event = TRUE;
                break;
            }
          }
        } else if (y > scrollbarv.b_bottom) {		/* pan down */
             ipan=1;
             dy=(0.1);
             gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
             dy=0.0;
             ipan=0;
        } else if (y < scrollbarv.b_top) {		/* pan up */
             ipan=1;
             dy=(-0.1);
             gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
             dy=0.0;
             ipan=0;
        }
        but_rlse = 2;    /* signal change in image as well as mouse  */
        return (but_rlse);
      } else if (xboxinside(scrollh,x,y)) {
        if (xboxinside(scrollbarh,x,y)) {
          vert=x;
          no_valid_event = TRUE;
          while ( no_valid_event) {
            XNextEvent(theDisp,event);
            switch (event->type) {
              case MotionNotify:	/* while mouse is moving track position  */
                x = event->xmotion.x;
                ipan=2;
                dx=((float)(vert-x)/(float)(scrollbarh.b_left-scrollbarh.b_right));
                gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
                dx=0.0;
                ipan=0;
                vert=event->xmotion.x;
/* update the network on the screen */
                break;
              case ButtonRelease:	/* button released so jump out of loop  */
                no_valid_event = FALSE;
                break;
              default:
                no_valid_event = TRUE;
                break;
            }
          }
        } else if (x > scrollbarh.b_right) {		/* pan right */
             ipan=1;
             dx=(0.1);
             gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
             dx=0.0;
             ipan=0;
        } else if (x < scrollbarh.b_left) {		/* pan left */
             ipan=1;
             dx=(-0.1);
             gridupdt_(&zoom,&grid_rz,&grid_oo,&snap,&ipan,&dx,&dy);
             dx=0.0;
             ipan=0;
        }
        but_rlse = 2;    /* signal change in image as well as mouse  */
        return (but_rlse);
      } else if (wire_avail >=1 && xboxinside(wire,x,y)) {

/* selected wire frame control */
        saved_font = menu_fnt; bottom = disp.b_top; left = wire_left;
        doitbox(wire,"image control",13,15,&saved_font,&box_fnt,&bottom,&left,"wire",'!');
      } else if(capture_avail >= 1 && xboxinside(capture,x,y)) {

/* capture image button */
        saved_font = current_font; bottom = disp.b_top; left = capture_left;
        doitbox(capture,"capture",7,9,&saved_font,&box_fnt,&bottom,&left,"capture",'!');
        but_rlse = 1;
      } else if(capture_avail >= 1 && xboxinside(captext,x,y)) {

/* capture text button */
        saved_font = current_font;
        bottom = fbb.b_bottom; left = captext_left;
        doitbox(capture,"capture",7,9,&saved_font,&box_fnt,&bottom,&left,"captext",'!');
        but_rlse = 1;
      } else if (azi_avail >=1 && xboxinside(aziplus,x,y)) {

/* selected azimuth +  control */
        saved_font = current_font; bottom = disp.b_top; left = aziplus_left;
        dosymbox(aziplus,2,&saved_font,&box_fnt,&bottom,&left,"aziplus",'!');
      } else if (azi_avail >=1 && xboxinside(aziminus,x,y)) {

/* selected azimuth -  control */
        saved_font = current_font; bottom = disp.b_top; left = aziminus_left;
        dosymbox(aziminus,2,&saved_font,&box_fnt,&bottom,&left,"aziminus",'!');
      } else if (azi_avail >=1 && xboxinside(elevplus,x,y)) {

/* selected elev +  control */
        saved_font = current_font; bottom = disp.b_top; left = elevplus_left;
        dosymbox(elevplus,2,&saved_font,&box_fnt,&bottom,&left,"elevplus",'!');
      } else if (azi_avail >=1 && xboxinside(elevminus,x,y)) {

/* selected elev -  control */
        saved_font = current_font; bottom = disp.b_top; left = elevminus_left;
        dosymbox(elevminus,2,&saved_font,&box_fnt,&bottom,&left,"elevminus",'!');
      } else if (setup_avail == 1 && xboxinside(setup,x,y)) {

/* selected fonts display */
        saved_font = current_font; bottom = b_setup; left = l_setup;
        doitbox(setup,"fonts  ",7,9,&saved_font,&box_fnt,&bottom,&left,"setup",'!');
      } else if (cpw_avail >=1 && xboxinside(cpw,x,y)) {

/* selected licence */
        saved_font = current_font; bottom = b_cpw; left = l_cpw;
        doitbox(cpw,"licence",7,9,&saved_font,&box_fnt,&bottom,&left,"copyright",'!');
      } else if (ocfgs >= 1 && xboxinside(cfgs,x,y)) {

/* If in active feature box cfgs (Context: site obstructions and/or ground) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgs,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgs.b_left+3,cfgs.b_bottom-2,"Context     ",12);
        avail_cfg = 's';
        cfgpk_(&avail_cfg,len_avail);	/* pass back zones to fortran */
        xbox(cfgs,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgs.b_left+3,cfgs.b_bottom-2,"Context     ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (ocfgnet >= 1 && xboxinside(cfgnet,x,y)) {

/* if in active config boxs cfgnet (Fluid flow) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgnet,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgnet.b_left+3,cfgnet.b_bottom-2,"Fluid flow  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
        avail_cfg = 'n';
        cfgpk_(&avail_cfg,len_avail);	/* pass back plant to fortran */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgnet,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgnet.b_left+3,cfgnet.b_bottom-2,"Fluid flow  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (ocfgpln >= 1 && xboxinside(cfgpln,x,y)) {

/* if in active config boxs cfgpln (HVAC) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgpln,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgpln.b_left+3,cfgpln.b_bottom-2,"HVAC        ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
        avail_cfg = 'p';
        cfgpk_(&avail_cfg,len_avail);	/* pass back plant to fortran */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgpln,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgpln.b_left+3,cfgpln.b_bottom-2,"HVAC        ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (ocfgeln >= 1 && xboxinside(cfgeln,x,y)) {

/* if in active config boxs cfgeln (Electrical) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgeln,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgeln.b_left+3,cfgeln.b_bottom-2,"Electrical  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
        avail_cfg = 'e';
        cfgpk_(&avail_cfg,len_avail);	/* pass back plant to fortran */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgeln,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgeln.b_left+3,cfgeln.b_bottom-2,"Electrical  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (ocfgc >= 1 && xboxinside(cfgc,x,y)) {

/* if in active config boxs cfgc (control) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgc,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgc.b_left+3,cfgc.b_bottom-2,"Control     ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
        avail_cfg = 'c';
        cfgpk_(&avail_cfg,len_avail);	/* pass back plant to fortran */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgc,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgc.b_left+3,cfgc.b_bottom-2,"Control     ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (ocfgren >= 1 && xboxinside(cfgren,x,y)) {

/* if in active config box cfgren (Renewables) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgren,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgren.b_left+3,cfgren.b_bottom-2,"Renewables  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
        avail_cfg = 't';
        cfgpk_(&avail_cfg,len_avail);	/* pass back cfd to fortran */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgren,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgren.b_left+3,cfgren.b_bottom-2,"Renewables  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (ocfgbeh >= 1 && xboxinside(cfgbeh,x,y)) {

/* if in active config box cfgbeh (Behaviour) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgbeh,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgbeh.b_left+3,cfgbeh.b_bottom-2,"Behaviour   ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
        avail_cfg = 'o';
        cfgpk_(&avail_cfg,len_avail);	/* pass back cfd to fortran */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgbeh,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgbeh.b_left+3,cfgbeh.b_bottom-2,"Behaviour   ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (ocfgsim >= 1 && xboxinside(cfgsim,x,y)) {

/* if in active config box cfgsim (Automation) */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgsim,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgsim.b_left+3,cfgsim.b_bottom-2,"Automation  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
        avail_cfg = 'a';
        cfgpk_(&avail_cfg,len_avail);	/* pass back cfd to fortran */
        saved_font = current_font;
        if (saved_font != disp_fnt) winfnt_(&disp_fnt);
        xbox(cfgsim,fg,white, BMCLEAR |BMEDGES);      /* invert box */
        XftDrawString8(draw, &xft_color,fst,cfgsim.b_left+3,cfgsim.b_bottom-2,"Automation  ",12);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (mouse_avail > 0 && xboxinside(mouse,x,y)) {

/* if in mouse box - help on mouse button clicks */
        saved_font = current_font;
        if (saved_font != butn_fnt) winfnt_(&butn_fnt);
        bh = f_height+2;	/* box height is font height +2 */
        msehbx.b_bottom = mouse.b_top - 4;
        msehbx.b_top = msehbx.b_bottom - bh;
        msehbx.b_right = mouse.b_right - 2;
        msehbx.b_left = msehbx.b_right - (f_width * 10);
        xbox(msehbx,fg,white, BMCLEAR);
        if (butid == 1) {
          xbox(mouse1,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
          XftDrawString8(draw, &xft_color,fst,msehbx.b_left+3,msehbx.b_bottom-2,(XftChar8 *) mseb1h,strlen(mseb1h));
        } else if (butid == 2) {
          xbox(mouse2,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
          bh = f_width*(10-strlen(mseb2h))/2;
          XftDrawString8(draw, &xft_color,fst,msehbx.b_left+3+bh,msehbx.b_bottom-2,(XftChar8 *) mseb2h,strlen(mseb2h));
        } else if (butid == 3) {
          xbox(mouse3,fg,ginvert, BMCLEAR | BMNOT |BMEDGES);      /* invert box */
          bh = f_width*strlen(mseb3h);
          XftDrawString8(draw, &xft_color,fst,msehbx.b_right-2-bh,msehbx.b_bottom-2,(XftChar8 *) mseb3h,strlen(mseb3h));
        }
        no_valid_event = TRUE;
        while (no_valid_event) {
          XNextEvent(theDisp,event);
          switch (event->type) {
            case ButtonRelease:
              no_valid_event = FALSE;
              break;
            default:
              break;
          }
        }
        xbox(msehbx,fg,white, BMCLEAR);
        if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
      } else if (xboxinside(viewbx,x,y)){

/* inside graphics display */
/* act depending on which button was pressed */

/* left mouse click */
/* if view controls are present, enable freelook until mouse is released */
        if (butid==1) {
          if (azi_avail >=1) {
            x_old=x;
            y_old=y;
            no_valid_event = TRUE;
            while (no_valid_event) {
              XNextEvent(theDisp,event);
              switch (event->type) {
                case MotionNotify: /* while mouse is moving track position  */
                  x = event->xmotion.x; y = event->xmotion.y;
                  idx=x-x_old; idy=y-y_old;
                  ifrlk=1;
                  if (abs(idx)>10) {
                    chgazi_(&idx,&ifrlk);
                    x_old=x;
                  }
                  if (abs(idy)>10) {
                    chgelev_(&idy,&ifrlk);
                    y_old=y;
                  }
                  break;
                case ButtonRelease:   /* button released so jump out of loop  */
                  idx=0;
                  ifrlk=0;
                  chgazi_(&idx,&ifrlk);
                  no_valid_event = FALSE;
                  break;
                default:
                  no_valid_event = TRUE;
                  break;
              }
            }
          }
          but_rlse=5;

/* right mouse click */
/* if view controls are present, enable pan until mouse is released */
        } else if (butid==3) {
          if (azi_avail >=1) {
            x_old=x;
            y_old=y;
            no_valid_event = TRUE;
            while (no_valid_event) {
              XNextEvent(theDisp,event);
              switch (event->type) {
                case MotionNotify: /* while mouse is moving track position  */
                  x = event->xmotion.x; y = event->xmotion.y;
                  idx=x-x_old; idy=y-y_old;
                  ifrlk=1;
                  if (abs(idx)>10 || abs(idy)>10) {
                    chgpan_(&idx,&idy,&ifrlk);
                    x_old=x; y_old=y;
                  }
                  break;
                case ButtonRelease:   /* button released so jump out of loop  */
                  idx=0; idy=0;
                  ifrlk=0;
                  chgpan_(&idx,&idy,&ifrlk);
                  no_valid_event = FALSE;
                  break;
                default:
                  no_valid_event = TRUE;
                  break;
              }
            }
          }
          but_rlse=5;

/* middle mouse click */
/* if view controls are present, return to optimum view bounds */
        } else if (butid==2) {
          if (azi_avail>=1) {
            optview_();
            no_valid_event = TRUE;
            while (no_valid_event) {
              XNextEvent(theDisp,event);
              switch (event->type) {
                case ButtonRelease:   /* button released so jump out of loop  */
                  no_valid_event = FALSE;
                  break;
                default:
                  no_valid_event = TRUE;
                  break;
              }
            }
          }
          but_rlse=5;

/* mouse wheel up/down */
/* if view controls are present, zoom in/out */
        } else if (butid==4 || butid==5) {
          if (azi_avail>=1) {
            idx=butid-3;
            chgzoom_(&idx);
            no_valid_event = TRUE;
            while (no_valid_event) {
              XNextEvent(theDisp,event);
              switch (event->type) {
                case ButtonRelease:   /* button released so jump out of loop  */
                  no_valid_event = FALSE;
                  break;
                default:
                  no_valid_event = TRUE;
                  break;
              }
            }
          }
          but_rlse=5;
        }

      }
    XftDrawDestroy(draw);
    return (but_rlse);
  }
  XftDrawDestroy(draw);
  return (but_rlse);
} /* aux_menu */

/* ********** refresh display *************** */
void refreshenv_()
{
   long int ifsc,itfsc,imfsc,lttyc,ltfont;           /* parameters must be  */
   long int menu_char,d_lines,displ_l,dialogue_l;  /* long ints to match  */
   long int gw,gh,gdw,gdh,g3w,g3h;                 /* fortran conventions */
   long int cl,cr,ct,cb,vl,vr,vt,vb;
   long int saved_font,avail;
   char blank[2];
   int i,j,len;	/* variables for text feedback redisplay */

/* re-establish how many lines that can be drawn (in case of resize) */
   saved_font = current_font;				/* save existing font */
   if (disp_fnt != saved_font) winfnt_(&disp_fnt);
   len = 124; 						/* max char width to print */
   disp_lines = (int) ((disp.b_bottom - disp.b_top) / (f_height+1));
   if (disp_lines != tfb_limtty) {
     tfb_line = tfb_line - tfb_limtty + disp_lines;
     tfb_limtty = disp_lines;
   }
   if (disp_fnt != saved_font) winfnt_(&saved_font); 	/* Restore font. */

/* Update position and size */
   XGetWindowAttributes(theDisp,win,&xwa);
   xsh.flags = (PPosition | PSize);
   xsh.height = xwa.height; xsh.width = xwa.width;
   xrt_width = xsh.width; xrt_height = xsh.height;  /* remember */
   xsh.x = xwa.x; xsh.y = xwa.y;
   XClearWindow(theDisp,win); /* clear the root window */

   menu_char=menu_offset;                        /* redraw feedbox  */
   d_lines=fbb_b_lines;
   feedbox_(&menu_char,&d_lines,&gw,&gh);

   if(dialogue_lines == 2) {                     /* if text feedback box redraw it */
      displ_l=disp_lines;
      dialogue_l=dialogue_lines;
      opengdisp_(&menu_char,&displ_l,&dialogue_l,&gdw,&gdh);
      strncpy(blank,"  ",2);
      msgbox_(blank,blank,strlen(blank),strlen(blank));      /* clear dialogue box  */
   }
   if(dbx1_avail == 1) {                       /* if graphic feedback clear it */
      cl=c3dcl; cr=c3dcr; ct=c3dct; cb=c3dcb;
      win3d_(&menu_char,&cl,&cr,&ct,&cb,&vl,&vr,&vt,&vb,&g3w,&g3h);
   }
   if(cpw_avail >= 1) {                     /* if copyright box redraw it */
     opencpw_();
   }
   if(capture_avail >= 1) {
     updcapt_(&avail);
   }
   if(setup_avail == 1) {                     /* if setup box redraw it */
     opensetup_();
   }
   if(edisp_index >= 1 && tfb_line >= 1) {	/* if feedback lines redraw them */
     disptext();
   }

/* pass back information to fortran common (as long int`s)
 * but only if using libesru_ter */
   ifsc=butn_fnt; itfsc=disp_fnt; imfsc=menu_fnt;
   lttyc=disp_lines;
   updview_(&ifsc,&itfsc,&imfsc,&vl,&vr,&vt,&vb,&g3w,&g3h,&lttyc);

/* If showing feature boxes call Fortran opencfg and get back indicators of */
/* which features are active in this model. The returned items are held globally. */
   if(cfg_boxs >= 0) {
     ltfont = butn_fnt;
     opencfg_(&cfg_boxs,&ocfgz,&ocfgs,&ocfgnet,&ocfgc,&ocfgpln,&ocfgeln,
              &ocfgren,&ocfgfab,&ocfgbeh,&ocfgsim);
   }
   return;
} /* refreshenv */

/* ******  Place licence button on screen ********** */
/* Place towards the right side of window and just above the base. */
void opencpw_()
{
 long int saved_font;
 int bottom, left;	/* pixel at lower left of box */
 int label_ht;		/* for height of the dialog box */
 int cpwfont;           /* smallest proportional font */
 saved_font = current_font; cpwfont=4;
 if (saved_font != butn_fnt) winfnt_(&butn_fnt);
 label_ht = f_height+4;
 cpw_avail = 1;             /* tell the world that licence is available */
 if (dialogue_lines != 0) { b_cpw = msgbx.b_top -6; } else { b_cpw = xrt_height -16; }
 l_cpw= xrt_width - ((f_width * 10) + 3);
 bottom = b_cpw; left = l_cpw;
 doitbox(cpw,"licence",7,9,&saved_font,&box_fnt,&bottom,&left,"copyright",'-');
 return;
} /* opencpw */

/* ******  Place model feature buttons on screen ********** */
void opencfg_(cfg_type,icfgz,icfgs,icfgnet,icfgc,icfgpln,icfgeln,icfgren,
              icfgfab,icfgbeh,icfgsim)
  long int *cfg_type;	/* type of problem configuration */
  long int *icfgz,*icfgs,*icfgnet,*icfgc,*icfgpln,*icfgeln;       /* toggles for model feature boxes */
  long int *icfgren,*icfgfab,*icfgbeh,*icfgsim; 
{
 XftDraw *draw;
 long int eyex,eyey,sym,sz;  /* centre for image symbols and symbol index and size */
 long int saved_font;
 int bh,hdl;
 int oocfgz = (int) *icfgz;	/* toggle for zones button */
 int oocfgs = (int) *icfgs;	/* toggle for Context button */
 int oocfgnet = (int) *icfgnet;	/* toggle for Fluid flown and/or CFD button */
 int oocfgc = (int) *icfgc;	/* toggle for Control button */
 int oocfgpln = (int) *icfgpln;	/* toggle for HVAC button */
 int oocfgeln = (int) *icfgeln;	/* toggle for Electrical network button */
 int oocfgren = (int) *icfgren; /* toggle for Renewables button */
 int oocfgfab = (int) *icfgfab;	/* toggle for Enhaanced fabric button */
 int oocfgbeh = (int) *icfgbeh;	/* toggle for Behaviour */
 int oocfgsim = (int) *icfgsim;	/* toggle for Automation button */
 ocfgz = oocfgz;  ocfgs = oocfgs; ocfgnet = oocfgnet; ocfgc = oocfgc;  /* remember toggles at global level */
 ocfgpln = oocfgpln; ocfgeln = oocfgeln; ocfgren = oocfgren;
 ocfgfab = oocfgfab; ocfgbeh = oocfgbeh; ocfgsim = oocfgsim;

 saved_font = current_font;
 if (saved_font != disp_fnt) winfnt_(&disp_fnt);
 cfg_boxs = *cfg_type;	/* tell the world that config boxes are available */

// Define local drawable for Xft font.
 draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  bh = f_height+2;	/* box height is font height +2 */
  hdl = viewbx.b_right - (f_width * 16);
  XftDrawString8(draw, &xft_color,fst,hdl,viewbx.b_top+bh-1,"Features",8);
  
  cfgs.b_top = viewbx.b_top + (1 * bh) +2;    cfgs.b_bottom = cfgs.b_top + bh;
  cfgs.b_right = viewbx.b_right - 2; cfgs.b_left = cfgs.b_right - (f_width * 16);
  xbox(cfgs,fg,white, BMCLEAR | BMEDGES);
  if (oocfgs >= 1) {	/* site */
    XftDrawString8(draw, &xft_color,fst,cfgs.b_left+3,cfgs.b_bottom-2, "Context     ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgs.b_left+3,cfgs.b_bottom-2,"Context     ",12);
  }
  cfgpln.b_top   = viewbx.b_top + (2 * bh) +5;   cfgpln.b_bottom = cfgpln.b_top + bh;
  cfgpln.b_right = viewbx.b_right - 2; cfgpln.b_left = cfgpln.b_right - (f_width * 16);
  xbox(cfgpln,fg,white, BMCLEAR | BMEDGES);
  if (oocfgpln >= 1) {	/* HVAC */
    XftDrawString8(draw, &xft_color,fst,cfgpln.b_left+3,cfgpln.b_bottom-2, "HVAC        ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgpln.b_left+3,cfgpln.b_bottom-2,"HVAC        ",12);
  }
  cfgeln.b_top   = viewbx.b_top + (3 * bh) +8;   cfgeln.b_bottom = cfgeln.b_top + bh;
  cfgeln.b_right = viewbx.b_right - 2; cfgeln.b_left = cfgeln.b_right - (f_width * 16);
  xbox(cfgeln,fg,white, BMCLEAR | BMEDGES);
  if (oocfgeln >= 1) {	/* Electrical */
    XftDrawString8(draw, &xft_color,fst,cfgeln.b_left+3,cfgeln.b_bottom-2, "Electrical  ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgeln.b_left+3,cfgeln.b_bottom-2,"Electrical  ",12);
  }
  cfgren.b_top   = viewbx.b_top + (4 * bh) +11;   cfgren.b_bottom = cfgren.b_top + bh;
  cfgren.b_right = viewbx.b_right - 2; cfgren.b_left = cfgren.b_right - (f_width * 16);
  xbox(cfgren,fg,white, BMCLEAR | BMEDGES);
  if (oocfgren >= 1) {	/* special materials */
    XftDrawString8(draw, &xft_color,fst,cfgren.b_left+3,cfgren.b_bottom-2, "Renewables  ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgren.b_left+3,cfgren.b_bottom-2,"Renewables  ",12);
  }
  cfgnet.b_top   = viewbx.b_top + (5 * bh) +14;   cfgnet.b_bottom = cfgnet.b_top + bh;
  cfgnet.b_right = viewbx.b_right - 2; cfgnet.b_left = cfgnet.b_right - (f_width * 16);
  xbox(cfgnet,fg,white, BMCLEAR | BMEDGES);
  if (oocfgnet >= 1) {	/* network */
    XftDrawString8(draw, &xft_color,fst,cfgnet.b_left+3,cfgnet.b_bottom-2, "Fluid flow  ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgnet.b_left+3,cfgnet.b_bottom-2,"Fluid flow  ",12);
  }
  cfgc.b_top   = viewbx.b_top + (6 * bh) +17;   cfgc.b_bottom = cfgc.b_top + bh;
  cfgc.b_right = viewbx.b_right - 2; cfgc.b_left = cfgc.b_right - (f_width * 16);
  xbox(cfgc,fg,white, BMCLEAR | BMEDGES);   /* draw the controls box */
  if (oocfgc >= 1) {	/* control */
    XftDrawString8(draw, &xft_color,fst,cfgc.b_left+3,cfgc.b_bottom-2, "Control     ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgc.b_left+3,cfgc.b_bottom-2,"Control    ",12);
  }
  cfgfab.b_top   = viewbx.b_top + (7 * bh) +20;   cfgfab.b_bottom = cfgfab.b_top + bh;
  cfgfab.b_right = viewbx.b_right - 2; cfgfab.b_left = cfgfab.b_right - (f_width * 16);
  xbox(cfgfab,fg,white, BMCLEAR | BMEDGES);
  if (oocfgfab >= 1) {	/* special materials */
    XftDrawString8(draw, &xft_color,fst,cfgfab.b_left+3,cfgfab.b_bottom-2, "Enhanced fabric",15);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgfab.b_left+3,cfgfab.b_bottom-2,"Enhanced fabric",15);
  }
  cfgbeh.b_top   = viewbx.b_top + (8 * bh) +23;   cfgbeh.b_bottom = cfgbeh.b_top + bh;
  cfgbeh.b_right = viewbx.b_right - 2; cfgbeh.b_left = cfgbeh.b_right - (f_width * 16);
  xbox(cfgbeh,fg,white, BMCLEAR | BMEDGES);
  if (oocfgbeh >= 1) {	/* high res occupant */
    XftDrawString8(draw, &xft_color,fst,cfgbeh.b_left+3,cfgbeh.b_bottom-2, "Behaviour   ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgbeh.b_left+3,cfgbeh.b_bottom-2,"Behaviour   ",12);
  }
  cfgsim.b_top   = viewbx.b_top + (9 * bh) +26;   cfgsim.b_bottom = cfgsim.b_top + bh;
  cfgsim.b_right = viewbx.b_right - 2; cfgsim.b_left = cfgsim.b_right - (f_width * 16);
  xbox(cfgsim,fg,white, BMCLEAR | BMEDGES);
  if (oocfgsim >= 1) {	/* high res occupant */
    XftDrawString8(draw, &xft_color,fst,cfgsim.b_left+3,cfgsim.b_bottom-2, "Automation  ",12);
  } else {
    XftDrawString8(draw, &xft_grey50,fst,cfgsim.b_left+3,cfgsim.b_bottom-2,"Automation  ",12);
  }


  if (saved_font != disp_fnt) winfnt_(&saved_font);  /* restore std font */
  XftDrawDestroy(draw);
  return;
} /* opencfg */

/* ******  Place setup button on screen ********** */
/* Place towards the right side of window and just above any dialogue box. */
void opensetup_()
{
 long int saved_font;
 int bottom, left;	/* pixel at lower left of box (supplied) */

 saved_font = current_font;
 if (saved_font != butn_fnt) winfnt_(&butn_fnt);
 setup_avail = 1;             /* tell the world that setup is available */
 l_setup = xrt_width - ((f_width * 10) + 3);
 if (dialogue_lines != 0) { b_setup = msgbx.b_top -24; } else { b_setup = xrt_height -34; }

 bottom = b_setup; left = l_setup;
 doitbox(setup,"fonts  ",7,9,&saved_font,&box_fnt,&bottom,&left,"setup",'-');
 return;
} /* opensetup */

/* ******  Notify level for wireframe button ********** */
// Typically passed the current number of zones. The logic only tests
// for non-zero state.  Zero would be initial state.
void updwire_(avail)
  long int *avail;
{
  long int saved_font;
  int bottom, left;	/* pixel at lower left of box (supplied) */

  if(wire_avail == 0 && *avail >= 0) {	/* probably first time in */
    saved_font = current_font;
    bottom = disp.b_top; left = wire_left;
    doitbox(wire,"image control",13,15,&saved_font,&box_fnt,&bottom,&left,"wire",'-');
    wire_avail = (int) *avail;         /* tell the world it is available */
  } else {
    wire_avail = (int) *avail;         /* tell the world it is available */
  }
  return;
} /* updwire_ */


/* ******  Notify level for capture button ********** */
void updcapt_(avail)
  long int *avail;
{
  long int saved_font;
  int bottom, left;

  if(capture_avail == 0 && *avail >= 0) {	/* probably first time in */
    saved_font = current_font;
    bottom = disp.b_top; left = capture_left;
    doitbox(capture,"capture",7,9,&saved_font,&box_fnt,&bottom,&left,"capture",'-');
    bottom = fbb.b_bottom; left = captext_left;
    doitbox(capture,"capture",7,9,&saved_font,&box_fnt,&bottom,&left,"captext",'-');
    capture_avail = (int) *avail;         /* tell the world it is available */
  } else {
    capture_avail = (int) *avail;         /* tell the world it is available */
  }
  return;
} /* updcapt_ */

/* ******  Notify level for azimuth button ********** */
void updazi_(avail)
  long int *avail;
{
  long int saved_font;
  int  bottom, left;

  if(azi_avail == 0 && *avail >= 0) {	/* probably first time in */
    saved_font = current_font;
    if (saved_font != butn_fnt) winfnt_(&butn_fnt);
    wire_left = disp.b_right - (f_width * 28);
    capture_left = disp.b_right - (f_width * 10);
    captext_left = disp.b_right - (f_width * 10);
    elevplus_left = disp.b_right - (f_width * 31);
    elevminus_left = disp.b_right - (f_width * 34);
    elev_left = disp.b_right - (f_width * 46);
    aziplus_left = disp.b_right - (f_width * 50);
    aziminus_left = disp.b_right - (f_width * 53);
    azi_left = disp.b_right - ((f_width * 63)+4);
    if (saved_font != butn_fnt) winfnt_(&saved_font);  /* restore std font */

    bottom = disp.b_top; left = aziplus_left;
    dosymbox(aziplus,2,&saved_font,&box_fnt,&bottom,&left,"aziplus",'-');

    bottom = disp.b_top; left = aziminus_left;
    dosymbox(aziminus,2,&saved_font,&box_fnt,&bottom,&left,"aziminus",'-');

    bottom = disp.b_top; left = azi_left;
    doitbox(azi,"azimuth",7,9,&saved_font,&box_fnt,&bottom,&left,"azi",'-');

    bottom = disp.b_top; left = elevplus_left;
    dosymbox(elevplus,2,&saved_font,&box_fnt,&bottom,&left,"elevplus",'-');

    bottom = disp.b_top; left = elevminus_left;
    dosymbox(elevminus,2,&saved_font,&box_fnt,&bottom,&left,"elevminus",'-');

    bottom = disp.b_top; left = elev_left;
    doitbox(elev,"elevation",9,11,&saved_font,&box_fnt,&bottom,&left,"elev",'-');

    if (saved_font != butn_fnt) winfnt_(&saved_font);  /* restore std font */
    azi_avail = (int) *avail;         /* tell the world it is available */
  } else {
    azi_avail = (int) *avail;         /* tell the world it is available */
  }

  return;
} /* updazi_ */

/* functions for passing .esprc capture commands */
/* capexgf - rectangular graphic capture feedback command */
void capexgf_(cmd,len_cmd)
  char      *cmd;         /* f77 message    */
  int  len_cmd;           /* length of string from f77    */
{
  int  l_m;
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

/* ******  Place mouse help button on screen ********** */
/* Place towards the right side of window and just above text feedback box. */
void openmouse_(mseb1,mseb2,mseb3,len1,len2,len3)
  char	*mseb1,*mseb2,*mseb3;
  int   len1,len2,len3;
/* local variables */
{
  XftDraw *draw;
  long int saved_font;
  int bh;
  int l_m1,l_m2,l_m3;

/* copy messages to C strings. */
  f_to_c_l(mseb1,&len1,&l_m1); strncpy(mseb1h,mseb1,(unsigned int)l_m1);	/* copy to static array */
  mseb1h[l_m1] = '\0';
  f_to_c_l(mseb2,&len2,&l_m2); strncpy(mseb2h,mseb2,(unsigned int)l_m2);	/* copy to static array */
  mseb2h[l_m2] = '\0';
  f_to_c_l(mseb3,&len3,&l_m3); strncpy(mseb3h,mseb3,(unsigned int)l_m3);	/* copy to static array */
  mseb3h[l_m3] = '\0';

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

  saved_font = current_font;
  if (saved_font != butn_fnt) winfnt_(&butn_fnt);
  mouse_avail = 1;             /* tell the world that mouse help is available */
  bh = f_height+2;	/* box height is font height +2 */
  mouse.b_top = dbx1.b_bottom - bh - bh;    /* double height box here */
  mouse.b_bottom = dbx1.b_bottom - 2;
  mouse.b_right = dbx1.b_right - 4;
  mouse.b_left = dbx1.b_right - (f_width * 8);
  xbox(mouse,fg,white, BMCLEAR | BMEDGES);
  XftDrawString8(draw, &xft_color,fst,mouse.b_left+2,mouse.b_bottom-2," mouse ",7);

/* Draw boxes representing buttons */
  mouse1.b_top = mouse.b_top + 1;
  mouse1.b_bottom = mouse.b_top + bh;
  mouse1.b_left = mouse.b_left + 2 ;
  mouse1.b_right = mouse1.b_left + bh;
  xbox(mouse1,fg,white, BMCLEAR | BMEDGES);
  mouse2.b_top = mouse.b_top + 1;
  mouse2.b_bottom = mouse.b_top + bh;
  mouse2.b_left = mouse1.b_right + 2;
  mouse2.b_right = mouse2.b_left + bh;
  xbox(mouse2,fg,white, BMCLEAR | BMEDGES);
  mouse3.b_top = mouse.b_top + 1;
  mouse3.b_bottom = mouse.b_top + bh;
  mouse3.b_right = mouse.b_right - 2;
  mouse3.b_left = mouse3.b_right - bh;
  xbox(mouse3,fg,white, BMCLEAR | BMEDGES);
  if (saved_font != butn_fnt) winfnt_(&saved_font);  /* restore std font */
  XftDrawDestroy(draw);

  return;
} /* openmouse */

/* notify pixel position of right edge of graphic display
   note that the fbb box structure contains int data types
   and these need to be explicitly cast to long int */
void findrtb_(long int* right,long int* top,long int* bottom)
{
  *bottom = (long int) fbb.b_bottom;
  *right = (long int) fbb.b_right;
  *top = (long int) fbb.b_top;
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
  XFlush(theDisp);
  XNextEvent(theDisp, &event);

  return (inpwait);
}
*/

/* ****** Switch network graphics ON or off ****** */
void nwkgflg_(ngf)
long int *ngf;
{
network_gpc= (int) *ngf;
}
/********* Switch between network drawing modes *********/
void nwksmod_(sel,con,dat)
long int *sel;
long int *con;
long int *dat;
{
nselect= (int) *sel;	/* if 1 then user can select/unselect icons */
nconnect=(int) *con;
ndata= (int) *dat;
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
/* Local variables   */
  XEvent event;
  KeySym     ks;
  Pixmap under;		        /* to save image under proforma  */
  XftDraw *draw;
  int  u_height, u_width;      /* size of the under pixmap */
  int  num,ipos;	/* length of lists    */
  int  eoffset;	/* pixel offset to edited text */
  long int saved_font, changed_font;
  int  mob_height,mob_width,mib_height,mib_width;	/* widths & height of outer & inner boxes */
  int  xb,yb,x,y,i,j,k,iy,lt1,lineheight,edhight;
  int  okbox_left,qbox_left;	/* position of ok box and query box */
  int  w_edit,c_edit;	/* which editing item is associated with current position */
  long int  ipflg,ishowmoreflg;	/* help paging if ipflg=1, showmore box if ishowmoreflg=1 */
  long int  uresp;		/* help response, normally 0 to end, 1 is showmore ok */
  long int imx,imy;	/* position for popup help */
  box menubx, gmenubx;	/* inner display, outer boxes */
  box tk_box[PROFMA_LEN];    /* array of tick boxes */
  box edit_box[PROFMA_LEN];    /* array of editing boxes */
  int lt2[PROFMA_LEN];	/* lengths of edit strings */
  int cwidth[PROFMA_LEN];	/* array of width (chars) for list (to position editing text) */
  int list2edit[PROFMA_LEN];	/* pointer from list position to index of editing string */
  int b_width;   /* b_width  pixels w/in editing box */
  int fitchars,offsc,x1,x2;  /* chars able to fit within editing box, chars between left
                                       of string & cursor */
  int lstrlen;
  int	initial_button_in = FALSE;
  Bool exp = 1;
  int	no_valid_event;
  static char sbuf[124];	/* editing buffer */
  static char buf[80];	/* keystroke buffer */
  static int blen = 0;	/* number of pending keystrokes */

/* local working string arrays */
char *locsstr = sstr;
int locnstr = (int) *nstr;
char *loctitle = title;
char *loclist = list;
int ilen = (int) *nlist;
char *loclisttypes = listtypes;
int iwhich;	/* local indicator of what has been selected */
int iwth = (int) *impcwth;	/* character width of proforma */

  if(ilen == 0){	/* don't bother if no list */
    if(XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
    return;
  }
  help_lines = (int) *nhelp; /* set number of help lines setup for this display */

  /* loop through text to display pass fortran array of strings into static
     display_list using same logic as updmenu_ */
  k = 0;
  for(num = 0; num < ilen; num++) {	/* for each display line...  */
    for(j = 0; j < lenlist; j++) {	/* for each character...  */
      display_list[num][j] = loclist[k];
      k = k +1;   /* increment for next char in items (a fortran string array does not have
                     nulls between strings in array, it just looks like one long string) */
    }
    display_list[num][lenlist] = '\0';	/* write terminator  */
    f_to_c_l(display_list[num],&lenlist,&cwidth[num]);	/* declared width and actual length of item */
    if (listact[num] < 0 || listact[num] > 1) listact[num] = 0;	/* if listact has
	been defined in the calling routine as 0 or 1 then use otherwise set to zero */
    list2edit[num] = 0;
/* debug  fprintf(stderr,"fprofma: width %d item %s\n",cwidth[num],display_list[num]); */
  }
  display_lines = ilen;

  k = 0;
  for(num = 0; num < locnstr; num++) {	/* for each edit string...  */
    for(j = 0; j < lensstr; j++) {	/* for each character...  */
      edit_list[num][j] = locsstr[k];
      k = k +1;   /* increment for next char in items (a fortran string array does not have
                     nulls between strings in array, it just looks like one long string) */
    }
    edit_list[num][lensstr] = '\0';	/* write terminator  */
    f_to_c_l(edit_list[num],&lensstr,&lt2[num]);	/* declared width and actual length of item */
  }
  edit_lines = locnstr;

  *ino = -1;	/* initial setting of parameter and local return index */
  iwhich = -1;

  xb = (int) *impx;  yb = (int) *impy;
  lt1 = (int) strlen(loctitle);  /* width of title */
  changed_font = 0;
  saved_font = current_font; /* save existing font  */

  /* having established a font, figure out heights and widths of outer and inner box */
  mib_height = (ilen+1)*(f_height+4);   /* inner pixel height */
  mob_height = mib_height + (f_height + 8 + 5);  /* include space for bottom buttons  */
  mib_width = (iwth*f_width);	/* inner pixel width */
  mob_width = mib_width +20;	/* outer box slightly wider */
  edhight = f_height;
/*
 If a specific box starting point has been passed then try to honor
 this otherwise place it slightly above the dialogue area.
*/
  if (xb == 0) {
    xb= 110;  yb = xsh.height-70;
  }

/* Create the outer and inner boxs. */
  gmenubx.b_top   = yb - mob_height;  gmenubx.b_bottom= yb;
  gmenubx.b_left  = xb;  gmenubx.b_right = xb + mob_width;
  okbox_left = gmenubx.b_right - ((2 * 5) + (f_width * 5));
  qbox_left = gmenubx.b_right - ((1 * 5) + (f_width * 2));
  menubx.b_top   = gmenubx.b_top + 5;
  menubx.b_bottom= gmenubx.b_bottom - (f_height + 8);
  menubx.b_left  = gmenubx.b_left + 5;  menubx.b_right = gmenubx.b_right - 5;
  lineheight = f_height+4;

  u_width = WIDTH(gmenubx); u_height = HEIGHT(gmenubx); /* width & height area to save */
  under = XCreatePixmap(theDisp,win,(unsigned int) u_width,(unsigned int) u_height,dispDEEP);
  box_to_pix(win,gmenubx,under,u_width,u_height);   /* save rect under gmenubx to under */

  for ( i = 0; i < ilen; i++ ) {	/* initial left and right tick box positions */
    tk_box[i].b_left = menubx.b_left + 5;
    tk_box[i].b_right = menubx.b_left + 15;
  }

// Define local drawable for Xft font.
  draw = XftDrawCreate(theDisp,win,theVisual,theCmap);

/* display outer and inner boxes and controls */
  xbox(gmenubx,fg,gpopfr,BMCLEAR |BMEDGES);	/* draw border box with edges  */
  xbox(gmenubx,fg,bg,BMEDGES);
  xbox(menubx,fg,white,BMCLEAR|BMEDGES);	/* draw inner box  */
  okbox("ok",2,3,&gmenubx.b_bottom,&okbox_left,'-');	/* draw ok box  */
  qbox_("?",1,2,&gmenubx.b_bottom,&qbox_left,'-');	/* draw ? box  */
  XftDrawString8(draw, &xft_color,fst,gmenubx.b_left+10,gmenubx.b_bottom-5,(XftChar8 *) loctitle,lt1);

/* build up display, note which are editable items.  */
  w_edit = -1;
  for ( i = 0; i < ilen; i++ ) {
    iy = menubx.b_top + ((i + 1) * (f_height+4)) + 5;
    if( loclisttypes[i] == 't' ) {  /* print text after tick */
      tk_box[i].b_bottom = iy; tk_box[i].b_top = iy -edhight;
      if ( listact[i] == 1 ) {
        xbox(tk_box[i],fg,white,BMCLEAR|BMEDGES);	/* draw open tick box  */
        xbox(tk_box[i],fg,ginvert, BMCLEAR | BMNOT );	/* then grey tick box  */
      } else if ( listact[i] == 0 ) {
        xbox(tk_box[i],fg,white,BMCLEAR|BMEDGES);	/* draw open tick box  */
      }
      XftDrawString8(draw, &xft_color,fst,menubx.b_left+20,iy,(XftChar8 *) display_list[i],cwidth[i]);
    } else if( loclisttypes[i] == 'e' ) {  /* print editing label and editing text */
      w_edit = w_edit +1;
      list2edit[i] = w_edit;
      XftDrawString8(draw, &xft_color,fst,menubx.b_left+5,iy,(XftChar8 *) display_list[i],cwidth[i]);
      eoffset = (cwidth[i] * f_width) + 5;
      edit_box[w_edit].b_bottom = iy+2; edit_box[w_edit].b_top = iy -edhight;
      edit_box[w_edit].b_left = menubx.b_left+eoffset;
      edit_box[w_edit].b_right = edit_box[w_edit].b_left + ( (int)swidth[w_edit] * f_width) +5;
      xbox(edit_box[w_edit],fg,white,BMCLEAR|BMEDGES);	/* draw edit box  */
      XftDrawString8(draw, &xft_color,fst,edit_box[w_edit].b_left+5,edit_box[w_edit].b_bottom-3,(XftChar8 *) edit_list[w_edit],lt2[w_edit]);
    } else if( loclisttypes[i] == '-' ) {
      XSetForeground(theDisp,theGC, ginvert);
      drawvwl(menubx.b_left + 15,iy-5,menubx.b_right - 15,iy-5,2);
      XSetForeground(theDisp,theGC, fg);
    } else if( listtypes[i] == 'l' ) {
      XftDrawString8(draw, &xft_color,fst,menubx.b_left+15,iy,(XftChar8 *) display_list[i],cwidth[i]);
    } else if( listtypes[i] == 'c' ) {
      XftDrawString8(draw, &xft_color,fst,menubx.b_left+5,iy,(XftChar8 *) display_list[i],cwidth[i]);
    } else {
      XftDrawString8(draw, &xft_color,fst,menubx.b_left+5,iy,(XftChar8 *) display_list[i],cwidth[i]);
    }
  }
  XFlush(theDisp); /* force drawing of text */
  no_valid_event = TRUE;
  while ( no_valid_event) {

/* << this might be place to put code for refreshing the display for visibility/configure notify >>
   << rather than repeated within the case statements >> */
    XNextEvent(theDisp, &event);
    switch (event.type) {
      case Expose:
        break;
      case VisibilityNotify:
/* debug      fprintf(stderr,"fprofma: vis event %d\n",event.xvisibility.state); */
        if(event.xvisibility.state == 0 ) {
          refreshenv_();
          xbox(gmenubx,fg,gpopfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
          xbox(gmenubx,fg,bg,BMEDGES);
          xbox(menubx,fg,white,BMCLEAR|BMEDGES);	/* draw inner box  */
          okbox("ok",2,3,&gmenubx.b_bottom,&okbox_left,'-');
          qbox_("?",1,2,&gmenubx.b_bottom,&qbox_left,'-');
          XftDrawString8(draw, &xft_color,fst,gmenubx.b_left+10,gmenubx.b_bottom-5,(XftChar8 *) loctitle,lt1);

/* rebuild display.  */
          for ( i = 0; i < ilen; i++ ) {
            iy = menubx.b_top + ((i + 1) * (f_height+4)) + 5;
            if( loclisttypes[i] == 't' ) {	/* print text after tick */
              tk_box[i].b_bottom = iy; tk_box[i].b_top = iy -edhight;
              if ( listact[i] == 1 ) {
                xbox(tk_box[i],fg,white,BMCLEAR|BMEDGES);	/* draw open tick box  */
                xbox(tk_box[i],fg,ginvert, BMCLEAR | BMNOT );	/* then grey tick box  */
              } else if ( listact[i] == 0 ) {
                xbox(tk_box[i],fg,white,BMCLEAR|BMEDGES);	/* draw open tick box  */
              }
              XftDrawString8(draw, &xft_color,fst,menubx.b_left+20,iy,(XftChar8 *) display_list[i],cwidth[i]);
            } else if( loclisttypes[i] == 'e' ) {	/* print editing label and editing text */
              w_edit = list2edit[i];
              XftDrawString8(draw, &xft_color,fst,menubx.b_left+5,iy,(XftChar8 *) display_list[i],cwidth[i]);
              eoffset = (cwidth[i] * f_width) + 5;
              edit_box[w_edit].b_bottom = iy+2; edit_box[w_edit].b_top = iy -edhight;
              edit_box[w_edit].b_left = menubx.b_left+eoffset;
              edit_box[w_edit].b_right = edit_box[w_edit].b_left + ((int)swidth[w_edit] * f_width) +5;
              xbox(edit_box[w_edit],fg,white,BMCLEAR|BMEDGES);	/* draw edit box  */
              XftDrawString8(draw, &xft_color,fst,edit_box[w_edit].b_left+5,edit_box[w_edit].b_bottom-3,(XftChar8 *) edit_list[w_edit],lt2[w_edit]);
            } else if( loclisttypes[i] == '-' ) {
              XSetForeground(theDisp,theGC, ginvert);
              drawvwl(menubx.b_left + 15,iy-5,menubx.b_right - 15,iy-5,2);
              XSetForeground(theDisp,theGC, fg);
            } else if( listtypes[i] == 'l' ) {
              XftDrawString8(draw, &xft_color,fst,menubx.b_left+15,iy,(XftChar8 *) display_list[i],cwidth[i]);
            } else if( listtypes[i] == 'c' ) {
              XftDrawString8(draw, &xft_color,fst,menubx.b_left+5,iy,(XftChar8 *) display_list[i],cwidth[i]);
            } else {
              XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) display_list[i],cwidth[i]);
            }
          }
          XFlush(theDisp);
        }
        break;
      case ConfigureNotify: /* user resized window, clear and then restore dialogue. */
/* debug       fprintf(stderr,"fprofma detected configure event\n");  */
        refreshenv_();
        xbox(gmenubx,fg,gpopfr,BMCLEAR |BMEDGES);	/* draw boarder box with edges  */
        xbox(gmenubx,fg,bg,BMEDGES);
        xbox(menubx,fg,white,BMCLEAR|BMEDGES);	/* draw inner box  */
        okbox("ok",2,3,&gmenubx.b_bottom,&okbox_left,'-');
        qbox_("?",1,2,&gmenubx.b_bottom,&qbox_left,'-');
        XftDrawString8(draw, &xft_color,fst,gmenubx.b_left+10,gmenubx.b_bottom-5,(XftChar8 *) loctitle,lt1);

/* rebuild display. */
        for ( i = 0; i < ilen; i++ ) {
          iy = menubx.b_top + ((i + 1) * (f_height+4)) + 5;
          if( loclisttypes[i] == 't' ) {  /* print text after tick */
            tk_box[i].b_bottom = iy; tk_box[i].b_top = iy -edhight;
            if ( listact[i] == 1 ) {
              xbox(tk_box[i],fg,white,BMCLEAR|BMEDGES);	/* draw open tick box  */
              xbox(tk_box[i],fg,ginvert, BMCLEAR | BMNOT );	/* then grey tick box  */
            } else if ( listact[i] == 0 ) {
              xbox(tk_box[i],fg,white,BMCLEAR|BMEDGES);	/* draw open tick box  */
            }
            XftDrawString8(draw, &xft_color,fst,menubx.b_left+20,iy,(XftChar8 *) display_list[i],cwidth[i]);
          } else if( loclisttypes[i] == 'e' ) {	/* print editing label and edit text */
            w_edit = list2edit[i];
            XftDrawString8(draw, &xft_color,fst,menubx.b_left+5,iy,(XftChar8 *) display_list[i],cwidth[i]);
            eoffset = (cwidth[i] * f_width) + 5;
            edit_box[w_edit].b_bottom = iy+2; edit_box[w_edit].b_top = iy -edhight;
            edit_box[w_edit].b_left = menubx.b_left+eoffset;
            edit_box[w_edit].b_right = edit_box[w_edit].b_left + ((int)swidth[w_edit] * f_width) +5;
            xbox(edit_box[w_edit],fg,white,BMCLEAR|BMEDGES);	/* draw edit box  */
            XftDrawString8(draw, &xft_color,fst,edit_box[w_edit].b_left+5,edit_box[w_edit].b_bottom-3,(XftChar8 *) edit_list[w_edit],lt2[w_edit]);
          } else if( loclisttypes[i] == '-' ) {
            XSetForeground(theDisp,theGC, ginvert);
            drawvwl(menubx.b_left + 15,iy-5,menubx.b_right - 15,iy-5,2);
             XSetForeground(theDisp,theGC, fg);
          } else if( listtypes[i] == 'l' ) {
            XftDrawString8(draw, &xft_color,fst,menubx.b_left+15,iy,(XftChar8 *) display_list[i],cwidth[i]);
          } else if( listtypes[i] == 'c' ) {
            XftDrawString8(draw, &xft_color,fst,menubx.b_left+5,iy,(XftChar8 *) display_list[i],cwidth[i]);
          } else {
            XftDrawString8(draw, &xft_color,fst,menubx.b_left+10,iy,(XftChar8 *) display_list[i],cwidth[i]);
          }
        }
        XFlush(theDisp);
        break;
      case ButtonPress:
        x = event.xbutton.x;  y = event.xbutton.y;
        if (xboxinside(menubx,x,y)){

/* User has clicked within the bounds of a item, if a tick type then hilight the box,
   if an editing entry invoke editing function. */
          i = (( y - menubx.b_top) / lineheight);
/* debug */  fprintf(stderr,"fprofma: i %d y %d lineheight %d\n",i,y,lineheight);
          if ( loclisttypes[i] == 't' ) {
            if ( listact[i] == 0 ) {
              xbox(tk_box[i],fg,ginvert, BMCLEAR | BMNOT );	/* draw grey tick box  */
              listact[i] = 1;	/* note that this tick box has been checked. */
            } else if ( listact[i] == 1 ) {
              xbox(tk_box[i],fg,white,BMCLEAR|BMEDGES);	/* draw open tick box  */
              listact[i] = 0;	/* note that this tick box has been checked. */
            }
            iwhich = i;
            no_valid_event = FALSE;	/* mark for return once logic sorted on recalling in fortran */
            XFlush(theDisp);
            Timer(300);
          } else if( loclisttypes[i] == '-' ) {
            Timer(300);
          } else if( loclisttypes[i] == 'l' ) {
            Timer(300);
          } else if( listtypes[i] == 'c' ) {	/* call back item selected */
            iwhich = i;
            Timer(300);
            listact[i] = 2;
            no_valid_event = FALSE;
          } else if (loclisttypes[i] == 'e') {
/* Edit function: use sbuf for editing, copy current edit string to it */
            c_edit = list2edit[i];
            if (xboxinside(edit_box[c_edit],x,y)){
              strncpy(sbuf,"                                                                                  ",82);
              strncpy(sbuf,edit_list[c_edit],(unsigned int)lt2[c_edit]);
/* debug fprintf(stderr,"i %d cur_ed %d lt2 %d swidth %l\n",i,c_edit,lt2[c_edit],swidth[c_edit]); */

              b_width = WIDTH(edit_box[c_edit]);
              fitchars = ((b_width - 5) / f_width);
              if ((lt2[c_edit]+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = lt2[c_edit]; }
              offsc = ((x - (edit_box[c_edit].b_left+5)) / f_width);
              x1 = (edit_box[c_edit].b_left+5) + (offsc * f_width);
              x2 = (edit_box[c_edit].b_left+5) + (lstrlen * f_width);
/* debug fprintf(stderr,"textfollow a: x %d y %d x1 %d x2 %d offsc %d %c\n",
                 x,y,x1,x2,offsc,sbuf[offsc]); */
              if( lstrlen == 1 ) {
                offsc = 1;
                x1 = (edit_box[c_edit].b_left+5) + (offsc * f_width);
              }
              if (x2 < x1 ) {
                offsc = lstrlen;
                x1 = (edit_box[c_edit].b_left+5) + (offsc * f_width);
              }
              update_edit_str(edit_box[c_edit],sbuf,&x1,&lstrlen);  /* re-draw askbx box and text */
              initial_button_in = TRUE;
              iwhich = i;	/* note which item is being edited */
              break;
            }
            XFlush(theDisp);
            Timer(900);
          }
          XSetForeground(theDisp,theGC, fg); XSetBackground(theDisp,theGC, bg);
   	  break;
        } else if (xboxinside(querb,x,y)){
           qbox_("?",1,2,&gmenubx.b_bottom,&querb.b_left,'!'); /* brief hilight */
	   imx = gmenubx.b_left-10; imy = gmenubx.b_top-25; ishowmoreflg = 0; uresp = 0;
           if (help_lines <= 20) ipflg = 0;
           if (help_lines > 20) ipflg = 1;
           egphelp_(&imx,&imy,&ipflg,&ishowmoreflg,&uresp);
           break;
        } else if (xboxinside(okb,x,y)){
          okbox("ok",2,3,&gmenubx.b_bottom,&okb.b_left,'!'); /* brief hilight */
          no_valid_event = FALSE;
          iwhich = ilen + 1;	/* indicate ok button pressed */
          break;
        }
      case KeyPress:	/* (XKeyEvent)&ev */
        if (initial_button_in) {	/* after each bit of editing update the string */
          blen = XLookupString((XKeyEvent*)&event,buf,80,&ks,(XComposeStatus *) NULL);
          if (ks==XK_Left || ks==XK_KP_4) {  /* left arrow pressed */
            if((x1-f_width) > edit_box[c_edit].b_left) {
              x1 = x1 - f_width; offsc = offsc - 1;
              update_edit_str(edit_box[c_edit],sbuf,&x1,&lstrlen);  /* clear & re-draw askbx box & text */
            } else {
             XBell(theDisp, 50);  /*  buf[0] is left arrow but at left edge  */
            }
          } else if (ks==XK_Right || ks==XK_KP_6) {  /* right arrow pressed */
            if((x1+f_width) < edit_box[c_edit].b_right) {
              x1 = x1 + f_width; offsc = offsc + 1;
              update_edit_str(edit_box[c_edit],sbuf,&x1,&lstrlen);  /* clear & re-draw askbx box & text */
            } else {
             XBell(theDisp, 50);  /* buf[0] is right arrow but at right edge */
            }
          }
          if(blen > 0) {
            if (buf[0] == '\r' || buf[0] == '\n' ) {	/* if return redraw box & text to remove bar */
              xbox(edit_box[c_edit],fg,white,BMCLEAR|BMEDGES);	/* draw edit box  */
              XftDrawString8(draw, &xft_color,fst,edit_box[c_edit].b_left+5,edit_box[c_edit].b_bottom-3,(XftChar8 *) edit_list[c_edit],lt2[c_edit]);
              initial_button_in = FALSE;
            } else if (buf[0] == '\177' || buf[0] == '\010') {  /*  buf[0] is DEL or BS  */
              if((x1-f_width) > edit_box[c_edit].b_left) {
/* debug fprintf(stderr,"track_edit_str *** sbuf %s char @ %c offsc %d\n",sbuf,sbuf[offsc],offsc);  */
                for (i=offsc; i<(int)swidth[c_edit]; i++) sbuf[i-1] = sbuf[i];  /* shift characters above offsc << */
                if(offsc <= lt2[c_edit]) {
                  sbuf[lt2[c_edit]]=' ';  /* last nonblank char becomes a space if | within string */
                  lt2[c_edit] = lt2[c_edit] - 1;  /* decrement lt2[c_edit] */
                }
                x1 = x1 - f_width; offsc = offsc - 1;  /* shift pointers */
/* debug fprintf(stderr,"track_edit_str now sbuf %s char @ %c offsc %d\n",sbuf,sbuf[offsc],offsc); */
                if ((lt2[c_edit]+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = lt2[c_edit]; }
                update_edit_str(edit_box[c_edit],sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
              } else {
                XBell(theDisp, 50);  /* buf[0] is DEL but at left edge  */
              }
            } else if (buf[0] == '\011') {
              XBell(theDisp, 50);
            } else if (buf[0] == '\033') {
              XBell(theDisp, 50);
            } else if (buf[0] >= ' ' && buf[0] < '\177') {
/* debug fprintf(stderr,"track_edit_str buf[0] is insert %c \n",buf[0]);   */
              if ((lt2[c_edit]+1) <= (int)swidth[c_edit]) {
                for (i=(int)swidth[c_edit]; i>offsc; i--) sbuf[i] = sbuf[i-1];  /* shift char above offsc >> */
                sbuf[offsc] = buf[0];  /* insert the new character */
                x1 = x1 + f_width; offsc = offsc + 1; lt2[c_edit] = lt2[c_edit] + 1;
/* debug fprintf(stderr,"track_edit_str now sbuf %s char @ %c %d %d \n",sbuf,sbuf[offsc],offsc,lt2[c_edit]);  */
                if ((lt2[c_edit]+2) > fitchars ) { lstrlen = fitchars; } else { lstrlen = lt2[c_edit]; }
                update_edit_str(edit_box[c_edit],sbuf,&x1,&lstrlen);  /* clear and re-draw askbx box and text */
              } else {
                XBell(theDisp, 50);
                fprintf(stderr,"editing: char string is full.\n");
              }
            }
            if(blen >1) fprintf(stderr,"track_edit_str 2nd buf char %c \n",buf[1]);
            if(blen >2) fprintf(stderr,"track_edit_str 3rd buf char %c \n",buf[2]);
          } else {
/* debug  fprintf(stderr,"track_edit_str nothing in buf \n");  */
          }	/* copy buffer back to edit_list */
          strncpy(edit_list[c_edit],sbuf,(unsigned int)lensstr);
        }
        break;
    }
  }

/* user has clicked on ok button or a call back function so: */
/* get edited text back into the original fortran array */
  ipos = 0;
  strncpy(locsstr,
  "                                                                         ",72);
  for(num = 0; num < locnstr; num++) {	/* for each edit string...  */
    strncpy(&locsstr[ipos],edit_list[num],(unsigned int)lensstr);	/* copy to local array */
    ipos=ipos+lensstr;
    locsstr[ipos] = '\0';	/* write terminator  */
  }
  *sstr = *locsstr;	/* copy locsstr back to sstr */
/* debug fprintf(stderr,"revised sstr %s \n",locsstr); */

/* clean up display */
  XClearArea(theDisp,win,gmenubx.b_left,gmenubx.b_top,(unsigned int) u_width,(unsigned int) u_height,exp);
  pix_to_box(under,u_width,u_height,gmenubx,win);
  XFreePixmap(theDisp, under);
  XftDrawDestroy(draw);
  if(XPending(theDisp) > 0) {
    while ( XPending(theDisp) > 0) {
      XNextEvent (theDisp,&event);	/* flush events */
    }
  }
  if (changed_font == 1) winfnt_(&saved_font);  /* Restore font.  */
  XFlush(theDisp); /* added to force draw */
  *ino = iwhich;	/* << >> set return value */
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
 * Function indicating if raw X!! libraries are used
 * use a one for X11 and in esru.c set two and in esru_fc.f set 3.
 */
int igraphiclib_()
{
  return 1;  /* use a one for X11 */
}

