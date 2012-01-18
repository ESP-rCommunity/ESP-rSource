/*
 * copyright ESRU 2001-11
 * This source may be copied, distributed, altered or used, but not sold
 * for profit.  This notice should remain in the source unaltered, and
 * any changes to the source made known to ESRU.
 * Used by esru_nox.c and esru_uitl.c
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
 * External Typing
 */

/* void	box_to_pix((Pixmap *),(box) frombox,(Pixmap *),int tw,int th); */
/* void	pix_to_box(Pixmap* from,int fw,int fh,box tobox,Pixmap* to); */
void	winlod_(char* name,long int* itime,long int* lix,long int* liy,int len);
void	winlodpart_(char* name,long int* lreqx,long int* lreqy,long int* lreqwidth,
	long int* lreqheight,long int* lix,long int* liy,long int* boxulx,
	long int* boxuly,long int* boxlrx,long int* boxlry,int len);
void	winfnt_(long int* n);
void	charsusingfnt_(long int* n, long int* cw, long int* nlines);
int	xboxinside(box b,int x,int y);
void	espad_(long int* llimit,long int* llimtty,long int* lline);
void	trackview_(long int* ichar,long int* irx,long int* iry);
void	ecirarc_(float* x,float* y,float* ths,float* thf,float* r,
	long int* num,float* di);
void	horaxishdw_(float* xmn,float* xmx,long int* offl,long int* offr,long int* offb,
	float* xadd,float* sca,long int* mode,long int* ind,long int* idiv,long int* isjday,
	char* msg,int mlen);
void	evwmenu_(char* titleptr,long int* impx,long int* impy,long int* iwth,long int* irpx,
	long int* irpy,long int* ino,long int* ipflg,long int* uresp,int len_title);
void	eewmenu_(char* titleptr,long int* impx,long int* impy,long int* iwth,long int* irpx,
	long int* irpy,long int* ino,long int* ipflg,long int* uresp,int len_title);
/* int	aux_menu(XEvent event); */
void	refreshenv_();
void	opencfg_(long int* cfg_type,long int* icfgz,long int* icfgn,long int* icfgc,
                 long int* icfgdfn,long int* iicfgz,long int* iicfgn,long int* iicfgc,long int* iicfgdfn);
void	findrtb_(long int* right,long int* top,long int* bottom);
void	fprofma_(char* sstr, long int* nstr, char* title, char* list, long int* nlist, char* listtypes,
		 long int* impx, long int* impy, long int* impcwth, long int swidth[], long int listact[],
		 long int* ino, long int* nhelp, int lensstr, int lentitle, int lenlist);

void	ipset(int flag);
void	f_to_c_l(char* msg,int* f_len,int* len);
void	wwcopen_(char* name,int len);
void	wwcclose_(char* name,int len);
void	wwcsetstart_();
void	wwcsetend_();
void	ckaccess_(long int* folder,long int* laccess,long int* lerr,char* fname,int len);
void	getfilelist_(char* folder,char* act,char* flist,long int nwflist[],long int* nflist,int lenfolder,int lenact,int lenflist);
void	curproject_(char* fcfgroot,char* fpath,char* fupath,char* fimgpth,char* fdocpth,char* ftmppth,long int* ibrowse,
        long int* iincomp,long int* iincon,int len_root,int len_fpath,int len_fupath,int len_fimgpth,int len_fdocpth,int len_ftmppth);
void    curviews_(float* EVX,float* EVY,float* EVZ,float* VX,float* VY,float* VZ,float* EAN,long int* JITZNM,long int* JITSNM,
        long int* JITVNO,long int* JITOBS,long int* JITSNR,long int* JITGRD,long int* JITORG,float* DIS,long int* JITBND,
        long int* JITDSP,long int* JITHLS,long int* JITHLZ,long int* JITPPSW);
void    pushgzonpik_(long int* jizgfoc,long int* jnzg);
void    pushnznog_(long int* jnznog,long int* jnznogv);
