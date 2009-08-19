/* Globally defined items for esp-r GTK interface */
#define	MENU_LIST_LEN	40

/* Sections of standard window */
GtkWidget *window;
GtkWidget *graphic;
GtkWidget *text;
GtkWidget *dialog;
GtkWidget *menu;	/* for the so-called static menu */
GtkWidget *emenu;	/* for dynamic menus */
GtkWidget *menu_frame;	/* frame holding dynamic menu within emenu */
GtkWidget *e_scrolled_window;	/* scrolled window for the dynamic menu within menu_frame */
GtkWidget *gtklist;	/* the list used for dynaimc menus */
GdkColormap *cmap;
GdkGC *gc;
gchar *help;

/* Context used by Pango for rendering text in the graphic window */
PangoContext* pango_context;
PangoFontMetrics *pango_metrics;

/* GdkPixmap *gr_image; Note: pixmaps are not static */
GdkPixmap *gr_image;

/* Backing pixmap for drawing area */
GdkPixmap *gr_image_old;

/* Backing pixmap for entire application (used to clear screen but that might not be necessary. */
GdkPixmap *entire_image;
GdkPixmap *entire_image_old;

/* Event loops and context */
GMainLoop *menu_loop;
//GMainLoop *pmenu_loop;


extern GdkColor cscale[49], zscale[100], gscale[49];

/* Information to pass back to fortran related to number of colours */
extern long int ncscale;  /* number of assigned colours in colour scale */
extern long int ngscale;  /* number of assigned colours in grey scale */
extern long int ngr;      /* number of assigned interface colours */
extern long int izc;      /* number of assigned zone colours */
extern long int mdepth;   /* to pass to fortran: sceen depth */
extern long int disp_fnt; /* font for text display box */
extern long int butn_fnt; /* button font size */
extern long int menu_fnt; /* preferred command menu font */

extern char *bgstr,  *whitestr, *blackstr; /* init default colors */
extern GdkColor fg, bg, bd, bw, white, black, infofg, infobg;	/* essential colors */
extern GdkColor gmenuhl, gpopfr, gfeedfr, ginvert, gmodbg, grey50, grey43;	/* interface colors */

extern gint ter;            /* terminal type passed on initial call */
extern gint child_ter;      /* child process terminal type  */

extern int wwc_ok;   /* assume no echo of drawing commands to wwc */
extern int wwc_macro;   /* assume not in a macro drawing command (etplot) */

extern gint xold, yold;     /* current 'pen' position used by eline_() */
extern gint x_off,y_off;         /* display X & Y offsets in pixels (see linescale) */
extern gfloat x_add,x_scale,y_add,y_scale;  /* X & Y axis scales and data offsets (see linescale) */
extern gint dash_on;         /* toggle for dashed section of a line in edash_() */
extern gint dash_rem;        /* remaining pixel length in incomplete dash in edash_() */
extern gint dbx1_avail;      /* flag for existance of graphic display box */
extern gint c3dcl,c3dcr,c3dct,c3dcb; /* dbx1 char offsets left, right, top, bottom */
extern int help_width;	/* current help max line character length */
extern int help_lines;	/* current number of active help lines */

extern int wire_avail;      /* wireframe adjustment holding nb of zones to draw */

extern char pm_list[MENU_LIST_LEN][125];	/* character arrays for menu buffer */
extern char pmtype_list[MENU_LIST_LEN];	/* character array representing m_list array use */
extern int pm_width;		/* current popup menu max line length */
extern int pm_lines;		/* current number of active popup menu lines */

extern char cappl[5];	/* f77 application name */
extern char cfgroot[25];	/* f77 project root name    */
extern char path[73];	/* f77 project path    */
extern char upath[73];	/* f77 users path    */
extern char imgpth[25];	/* f77 relative path to images    */
extern char docpth[25];	/* f77 relative path to documents    */
extern char tmppth[25];	/* f77 relative path to scratch folder    */
extern char capt_wf_exe[73];	/* command to execute for capture wire frame */
extern char capt_tf_file[73];	/* file for text feedback buffer dump */
extern char capt_all_exe[73];	/* command for capture all of display */
extern int browse;	/* if = 0 then user owns, if = 1 user browsing */
extern gint xrt_width, xrt_height;  /* same as xsh.width and xsh.height */

extern gint menu_pix_wd;	/* pixel width of initial menu (based on nb of characters *imenuchw) */
extern long int menuchw;	/* char width of initial menu (from fortran) */

/* Shared functions from esp-r.c */
void	destroy( GtkWidget *widget,gpointer data);
void	calculate_font_metrics( void);
void	winfin_();
void	egphelp_disp ( void);
void	createwin_ (long int *width,long int *height,long int *imenuchw,long int *ilimtty,char *head,int lhead);
void	setcscale_();
void	clrcscale_();
void	setgscale_();
void	clrgscale_();
void	setzscale_();
void	updwire_(long int* avail);
void	foundcolour_(long int* md,long int* nic,long int* ncs,long int* ngs,long int* nzc);
void	winclr_();
void	win3d_(long int* menu_char,long int* cl,long int* cr,long int* ct,long int* cb,
	long int* vl,long int* vr,long int* vt,long int* vb,long int* gw,long int* gh);
void	startbuffer_();
void	forceflush_();

/* Shared functions in esp_ask.c */
void	askdialog_(char *q1,char *reply,long int *ier,int lenq1,int lenrep);
void	askdialogcmd_(char *q1,char *reply,char *cmd,long int *ier,int lenq1,int lenrep,int lencmd);
void	askdialogcncl_(char *q1,char *reply,char *cncl,long int *ier,int lenq1,int lenrep,int lencmd);
void	askdialog2cmd_(char *q1,char *reply,char *cmd1,char *cmd2,long int *ier,int lenq1,int lenrep,int lencmd1,int lencmd2);
void	askreal_(char *q1, float *reply,long int *ier,int lenq1);
void	askint_(char *q1, long int *reply,long int *ier,int lenq1);
void	askdialog248_(char *q1,char *reply,long int *ier,int lenq1,int lenrep);

/* Shared functions in esru.c */
void	tchild_(long int* state);
void	Timer(int msec);
void	pausems_(long int* msec);
void	pauses_(long int* is);
void	f_to_c_l(char* msg,int* f_len,int* len);
void	wwcopen_(char* name,int len);
void	wwcclose_(char* name,int len);
void	wwcsetstart_();
void	wwcsetend_();
void	ckaccess_(long int* folder,long int* laccess,long int* lerr,char* fname,int len);
void	getfilelist_(char* folder,char* act,char* flist,long int nwflist[],long int* nflist,int lenfolder,int lenact,int lenflist);
void	curproject_(char* fcfgroot,char* fpath,char* fupath,char* fimgpth,char* fdocpth,char* ftmppth,long int* ibrowse,
        int len_root,int len_fpath,int len_fupath,int len_fimgpth,int len_fdocpth,int len_ftmppth);
void	curmodule_(char* fcmodule,int len_fcmodule);
void	capexgf_(char* cmd,int len_cmd);
void	capextf_(char* cmd,int len_cmd);
void	capexall_(char* cmd,int len_cmd);
void	userfonts_(long int* ifs,long int* itfs,long int* imfs);
int ixavail(); 

/* Shared functions in esp_draw.c */
void	wstxpt_(long int* x,long int* y,char* buff,int len);
void	textatxy_(long int* x,long int* y,char* buff,char* act,long int *n,int len);
void	textpixwidth_(char* buff,long int* pixelwidth,int len);
void	viewtext_(char* msg,long int* linep,long int* side,long int* size,int len);
void	findviewtext_(long int* charposp,long int* linep,long int* size,long int* irx,
	long int* iry);
void	drawswl(int xa,int ya,int xb,int yb);
void	esymbol_(long int* x,long int* y,long int* sym,long int* size);
void	drawpoint(int xa,int ya);
void	eline_(long int* x,long int* y,long int* operation);
void	edline_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis);
void	edash_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis);
void 	eswline_(long int* x1,long int* y1,long int* x2,long int* y2);
void	edwline_(long int* x1,long int* y1,long int* x2,long int* y2);
void	echain_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis);
void	drawdwl(int xa,int ya,int xb,int yb);
void	drawvwl(int xa,int ya,int xb,int yb,int width);
void	drawddash(int xa,int ya,int xb,int yb,int width);
void	drawoodash(int xa,int ya,int xb,int yb,int width);
void	axiscale_(long int* gw,long int* gh,float* xmn,float* xmx,float* ymn,
	float* ymx,float* xsc,float* ysc,float* sca,float* xadd,float* yadd);
void	linescale_(long int* loff,float* ladd,float* lscale,long int* boff,float* badd,
	float* bscale);
void	u2pixel_(float* ux,float* uy,long int* ix,long int* iy);
void	pixel2u_(long int* ux,long int* uy,float* gx,float* gy);
void 	etplot_(float* ux,float* uy,long int* updown,long int* sym);
void	erectan_(float* x,float* y,float* dx,float* dy,float* dt);
void	egrbox_(float* x,float* y,float* dx,float* dy,long int* gp);
void 	etriang_(float* x,float* y,float* dx,float* dy,float* dt);
void	ecirc_(long int* x,long int* y,long int* rad,long int* operation);
void	earc_(long int* x,long int* y,long int* rad,long int* ang1,
	long int* ang2,long int* operation);
void	dinterval_(float* v1,float* v2,float* dv,long int* ndec,long int* mode);
void	etlabel_(char* msg,float* x,float* y,long int* ipos,long int* size,int len);
void	vrtaxis_(float *ymn,float *ymx,long int *offl,long int *offb,long int *offt,
	float *yadd,float *sca,long int *mode,long int *side,char* msg,int mlen);
void	horaxis_(float *xmn,float *xmx,long int *offl,long int *offr,long int *offb,
	float *xadd,float *sca,long int *mode,char* msg,int mlen);
void	horaxishdw_(float *xmn,float *xmx,long int *offl,long int *offr,long int *offb,
	float *xadd,float *sca,long int *mode,long int *ind,long int *idiv,long int *isjday,char* msg,int mlen);
void	labelstr(long int *n,float *val,long int *WticC,char* sstr);
void	popupimage_(char *prom,char *docu,char *act,char *file,int lenprom,int lendocu,int lenact,int lenfile);

/* Shared functions in esp_menu.c */
void	espmenuinit_ (char *title, int len);
void	updmenu_(char* items,char* itypes,long int* nitmsptr,long int* iw,int len_items);
void	espmenuitems_ (char *item,long int *ino, int len); 
void	espmenu_ (int *ino); 
void	espabcbox_ (char *msg1, char *aopt, char *bopt, char *copt, 
          char *dopt, char *eopt, char *fopt, char *gopt,long int *ipick,
          int msg1_len, int aopt_len, int bopt_len, int copt_len,
          int dopt_len, int eopt_len, int fopt_len,int gopt_len);
void	espdozenbox_ (char *msg1, char *aopt, char *bopt, char *copt, 
          char *dopt, char *eopt, char *fopt, char *gopt,
          char *hopt, char *iopt, char *jopt, char *kopt,
          char *lopt, long int *ipick,
          int msg1_len, int aopt_len, int bopt_len, int copt_len,
          int dopt_len, int eopt_len, int fopt_len, int gopt_len,
          int hopt_len, int iopt_len, int jopt_len, int kopt_len,
          int lopt_len);

/* Shared functions esp_text.c */
void	inserttext_ ( char *message , int len);
void	msgbox_(char* msg1,char* msg2,int len1,int len2);
void	continuebox_ (char *msg1, char *cnt, int len1, int lencnt); 
void	clrhelp_();
void	updhelp_(char* message,int len);
void	egphelpscroll_(long int* impx,long int* impy,long int* ipflg,
	long int* ishowmoreflg,long int* uresp);

/* Shared functions still to be ported across from esru_nox.c */
/* void	windcl_(int* n, int* ir, int* ig, int* ib); */
/* void	winscl_(char* act, long int* n); */

int	ixavail();
int	igraphiclib();
