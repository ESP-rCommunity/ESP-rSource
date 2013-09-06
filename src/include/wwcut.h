/*
 * copyright ESRU 2001-9
 * This source may be copied, distributed, altered or used, but not sold
 * for profit.  This notice should remain in the source unaltered, and
 * any changes to the source made known to ESRU.
 */

/* #if defined(c_plusplus)||defined(__GNUG__)||defined(__GNUC__) */
/* # define PARMS  full function declaration prototypes accepted */
/* #endif */

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif /* TRUE */
# define NULLPTR(x)	((x *)0)	/* null (coerced) */

typedef unsigned char byte;

/*
 * structure that holds the corners of a box
 */
typedef struct _box{
	int	b_left;
	int	b_top;
	int	b_right;
	int	b_bottom;
}box;
/*
 * define box arguments that can be or-ed together
 */
#define BMCLEARALL	01	/* clear box, including edges */
#define BMCLEAR		02	/* clear inside of box, not including edges */
#define BMNOTALL	04	/* not all of box */
#define BMNOT		010	/* not inside of box */
#define BMEDGES		020	/* draw edges of box, using dd->d_line */
/* 
 * define wwcut arguments
 */
#define IPON		1
#define IPOFF		2
#define	NFONTS 		6
#define	HELP_LIST_LEN	60
#define	EDISP_LIST_LEN	500
#define	MENU_LIST_LEN	40
#define PROFMA_LEN	40
#define STARTX		100
#define STARTY		100
#define	TEKX		1024
#define	TEKY		780
#define	BORDER	2
#define	FONT	"fixed"
#define CDIST(x,y,z)  ((x)*(x) + (y)*(y) + (z)*(z))
#define HEIGHT(b)	(b.b_bottom-b.b_top+1)
#define WIDTH(b)	(b.b_right-b.b_left+1)

/* values of haveStdCmap */
#define STD_NONE     0        /* no stdcmap currently defined */
#define STD_111      1        /* 1/1/1 stdcmap is available */
#define STD_222      2        /* 2/2/2 stdcmap is available */
#define STD_232      3        /* 2/3/2 stdcmap is available */
#define STD_666      4        /* 6x6x6 stdcmap is available */
#define STD_332      5        /* 3/3/2 stdcmap is available */

/* values of colorMapMode */
#define CM_NORMAL    0        /* normal RO or RW color allocation */
#define CM_PERFECT   1        /* install own cmap if necessary */
#define CM_OWNCMAP   2        /* install own cmap always */
#define CM_STDCMAP   3        /* use stdcmap */

/* global data */

/*
 * External Typing:
 */
void	sizeint_(long int* size,long int* ulx,long int* uly);
void	sizehwxy_(long int* sizeh,long int* sizew,long int* ulx,long int* uly);
/* void	box_to_pix((Pixmap *),(box) frombox,(Pixmap *),int tw,int th); */
/* void	pix_to_box(Pixmap* from,int fw,int fh,box tobox,Pixmap* to); */
void	winlod_(char* name,long int* itime,long int* lix,long int* liy,int len);
void	winlodpart_(char* name,long int* lreqx,long int* lreqy,long int* lreqwidth,
	long int* lreqheight,long int* lix,long int* liy,long int* boxulx,
	long int* boxuly,long int* boxlrx,long int* boxlry,int len);
void	jwinit_(long int* term,int msglen,char* msg);
void	setcscale_();
void	clrcscale_();
void	setgscale_();
void	clrgscale_();
void	setzscale_();
void	foundcolour_(long int* md,long int* nic,long int* ncs,long int* ngs,long int* nzc);
void	userfonts_(long int* ifs,long int* itfs,long int* imfs);
void	winfin_();
void	winclr_();
void	windcl_(int* n, int* ir, int* ig, int* ib);
void	winscl_(char* act, long int* n);
void	winenqcl_(char* act, long int* n, long int* xcolid);
void	f_to_c_l(char* msg,int* f_len,int* len);
void	wwcopen_(char* name,int len);
void	wwcclose_(char* name,int len);
void	wwcsetstart_();
void	wwcsetend_();
void	ckaccess_(long int* folder,long int* laccess,long int* lerr,char* fname,int len);
void	getfilelist_(char* folder,char* act,char* flist,long int nwflist[],long int* nflist,int lenfolder,int lenact,int lenflist);
void	getfileslist_(char* folder,char* act,long int* nflist,int lenfolder,int lenact);
void	curproject_(char* fcfgroot,char* fpath,char* fupath,char* fimgpth,char* fdocpth,long int* ibrowse,
        long int* iincomp,long int* iincon,int len_root,int len_fpath,int len_fupath,int len_fimgpth,int len_fdocpth);
void    curviews_(float* EVX,float* EVY,float* EVZ,float* VX,float* VY,float* VZ,float* EAN,long int* JITZNM,long int* JITSNM,
        long int* JITVNO,long int* JITOBS,long int* JITSNR,long int* JITGRD,long int* JITORG,float* DIS,long int* JITBND,
        long int* JITDSP,long int* JITHLS,long int* JITHLZ,long int* JITPPSW);
void    pushgzonpik_(long int* jizgfoc,long int* jnzg);
void    pushnznog_(long int* jnznog,long int* jnznogv);
void	curmodule_(char* fcmodule,int len_fcmodule);
void	wstxpt_(long int* x,long int* y,char* buff,int len);
/* void	textatxy_(long int* x,long int* y,char* buff,long int* xcolid,int len); */
void	textatxy_(long int* x,long int* y,char* buff,char* act,long int* n,int len);
void	textpixwidth_(char* buff,long int* pixelwidth,int len);
void	winfnt_(long int* n);
void	charsusingfnt_(long int* n, long int* cw, long int* nlines);
void	xbox(box b, unsigned long fgc, unsigned long bgc, int flags);
int	xboxinside(box b,int x,int y);
void	feedbox_(long int* menu_char,long int* d_lines,long int* gw,long int* gh);
void	win3d_(long int* menu_char,long int* cl,long int* cr,long int* ct,long int* cb,
	long int* vl,long int* vr,long int* vt,long int* vb,long int* gw,long int* gh);
void	win3dclr_();
void	viewtext_(char* msg,long int* linep,long int* side,long int* size,int len);
void	findviewtext_(long int* charposp,long int* linep,long int* size,long int* irx,
	long int* iry);
void	etlabel_(char* msg,float* x,float* y,long int* ipos,long int* size,int len);
void	forceflush_();
void	Timer(int msec);
void	pausems_(long int* msec);
void	pauses_(long int* is);
void	drawswl(int xa,int ya,int xb,int yb);
void	esymbol_(long int* x,long int* y,long int* sym,long int* size);
void	qbox_(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act);
void	dbox(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act);
void	okbox(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act);
void	doitbox(box dobox,char* msg,int msglen,int asklen,long int* sav_font,long int* use_font,
	int* b_bottom,int* b_left,char* topic,char act);
void	dosymbox(box dobox,int asklen,long int* sav_font,long int* use_font,int* b_bottom,
	int* b_left,char* topic,char act);
void	altbox(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act);
void	alt2box(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act);
void	abcdboxs(char* msg,int msglen,int asklen,int* b_bottom,int* b_left,char act);
void	openaskbox_(char* msg1,char* msg2,long int* asklen,int len1,int len2);
void	openaskaltbox_(char* msg1,char* msg2,char* alt,long int* asklen,int len1,
	int len2,int len3);
void	openaskcnclbox_(char* msg1,char* msg2,char* cncl,long int* asklen,int len1,
	int len2,int len3);
void	openask2altbox_(char* msg1,char* msg2,char* alt,char* alt2,long int* asklen,int len1,
	int len2,int len3,int len4);
void	update_edit_str(box ebox,char* edstr,int* xbar,int* lstrlen);
/* void	track_edit_str(char[] sbuf,XEvent event,int* tx1,int* tf_len,int* tlen,
	int* tfitchars,int* toffsc,int* tno_valid_event); */
void	updhelp_(char* items,long int* nitmsptr,long int* iw,int len_items);
void	egphelp_(long int* impx,long int* impy,long int* ipflg,
	long int* ishowmoreflg,long int* uresp);
void	askdialog_(char* sstr,long int* id,long int* iq,int f_len);
void	askaltdialog_(char* sstr,char* alt,long int* id,long int* iq,int f_len,int a_len);
void	askcncldialog_(char* sstr,char* cncl,long int* id,long int* iq,int f_len,int a_len);
void	ask2altdialog_(char* sstr,char* alt,char* alt2,long int* id,long int* iq,
	int f_len,int a_len,int b_len);
void	msgbox_(char* msg1,char* msg2,int len1,int len2);
void	abbox_(char* msg1,char* msg2,char* opta,char* optb,long int* ok,
	int len1,int len2,int len3,int len4);
void	abcdefbox_(char* msg1,char* msg2,char* opta,char* optb,char* optc,char* optd,
	char* opte,char* optf,char* optg,long int* ok,int len1,int len2,int len3,
	int len4,int len5,int len6,int len7,int len8,int len9);
void	opengdisp_(long int* menu_char,long int* displ_l,long int* dialogue_l,
	long int* gdw,long int* gdh);
void	egdisp_(char* msg,long int* line,int len);
void	espad_(long int* llimit,long int* llimtty,long int* lline);
void	egdispclr_();
void	trackview_(long int* ichar,long int* irx,long int* iry);
void	eline_(long int* x,long int* y,long int* operation);
void	u2pixel_(float* ux,float* uy,long int* ix,long int* iy);
void 	etplot_(float* ux,float* uy,long int* updown,long int* sym);
void	edline_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis);
void	drawdwl(int xa,int ya,int xb,int yb);
void	edwline_(long int* x1,long int* y1,long int* x2,long int* y2);
void 	eswline_(long int* x1,long int* y1,long int* x2,long int* y2);
void	drawvwl(int xa,int ya,int xb,int yb,int width);
void	edash_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis);
void	echain_(long int* x1,long int* y1,long int* x2,long int* y2,long int* ipdis);
void	drawddash(int xa,int ya,int xb,int yb,int width);
void	drawoodash(int xa,int ya,int xb,int yb,int width);
void	drawpoint(int xa,int ya);
void	erectan_(float* x,float* y,float* dx,float* dy,float* dt);
void	egrbox_(float* x,float* y,float* dx,float* dy,long int* gp);
void 	etriang_(float* x,float* y,float* dx,float* dy,float* dt);
void	ecirarc_(float* x,float* y,float* ths,float* thf,float* r,
	long int* num,float* di);
void	ecirc_(long int* x,long int* y,long int* rad,long int* operation);
void	earc_(long int* x,long int* y,long int* rad,long int* ang1,
	long int* ang2,long int* operation);
void	axiscale_(long int* gw,long int* gh,float* xmn,float* xmx,float* ymn,
	float* ymx,float* xsc,float* ysc,float* sca,float* xadd,float* yadd);
void	linescale_(long int* loff,float* ladd,float* lscale,long int* boff,float* badd,
	float* bscale);
void	dinterval_(float* v1,float* v2,float* dv,long int* ndec,long int* mode);
void	labelstr(long int* n,float* val,long int* WticC,char* sstr);
void	vrtaxis_(float* ymn,float* ymx,long int* offl,long int* offb,long int* offt,
	float* yadd,float* sca,long int* mode,long int* side,char* msg,int mlen);
void	horaxis_(float* xmn,float* xmx,long int* offl,long int* offr,long int* offb,
	float* xadd,float* sca,long int* mode,char* msg,int mlen);
void	horaxishdw_(float* xmn,float* xmx,long int* offl,long int* offr,long int* offb,
	float* xadd,float* sca,long int* mode,long int* ind,long int* idiv,long int* isjday,
	char* msg,int mlen);
void	updmenu_(char* items,char* itypes,long int* nitmsptr,long int* iw,int len_items);
void	evwmenu_(char* titleptr,long int* impx,long int* impy,long int* iwth,long int* irpx,
	long int* irpy,long int* ino,long int* ipflg,long int* uresp,int len_title);
void	eewmenu_(char* titleptr,long int* impx,long int* impy,long int* iwth,long int* irpx,
	long int* irpy,long int* ino,long int* ipflg,long int* uresp,int len_title);
/* int	aux_menu(XEvent event); */
void	refreshenv_();
void	opencpw_();
void	opencfg_(long int* cfg_type,long int* icfgz,long int* icfgn,long int* icfgc,
                 long int* icfgdfn,long int* iicfgz,long int* iicfgn,long int* iicfgc,long int* iicfgdfn);
void	opensetup_();
void	updwire_(long int* avail);
void	updcapt_(long int* avail);
void	updazi_(long int* avail);
void	capexgf_(char* cmd,int len_cmd);
void	capextf_(char* cmd,int len_cmd);
void	capexall_(char* cmd,int len_cmd);
void	findrtb_(long int* right,long int* top,long int* bottom);
void	openmouse_(char* mseb1,char* mseb2,char* mseb3,int len1,int len2,int len3);
void	movemse();
void	closemouse_();
void	drscrollbar();
void	disptext();
void	fprofma_(char* sstr, long int* nstr, char* title, char* list, long int* nlist, char* listtypes,
		 long int* impx, long int* impy, long int* impcwth, long int swidth[], long int listact[],
		 long int* ino, long int* nhelp, int lensstr, int lentitle, int lenlist);

void	ipset(int flag);
int	ixavail();
int	igraphiclib();
