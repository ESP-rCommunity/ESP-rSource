/*
 * copyright Mark M Martin. RAL. 1986
 * This source may be copied, distributed, altered or used, but not sold
 * for profit. It is not in the public domain. This notice should remain
 * in the source unaltered, and any changes to the source made by
 * people other than the author should be marked as such.
 */
/*
 * C include file for window information in ww library.
 * 28 June 85. 28 July 85. 24 Oct 85. etc
 * Beware: automatic extraction of #defines and define comments for man page.
 * # define will not be found. * define wiil be.
 * define Useful constants, machine type, and return codes.
# define WWFORX from makefile?
 */
#ifndef _WWINFO_H
# define _WWINFO_H

#ifdef mips
#   define WWFORX
#   ifdef ultrix
#      define WWFORXDEC
#   else /* ultrix */
#      define WWFORXWC
#   endif /* ultrix */
#endif /* mips */

#ifdef stellar
#   define WWFORX
#   define WWFORSYSTEMV
#endif /* stellar */

#ifndef WWFORX
#   ifdef mc68000
#      define WWFORSUN
#      define WWFORCOLOUR
#   endif /* mc68000 */
#   ifdef sparc
#      define WWFORSUN
#      define WWFORCOLOUR
#   endif /* sparc */
#else /* WWFORX */
#   define WWFORCOLOUR
#   ifdef mc68000
#      define WWFORXSUN
#   endif /* mc68000 */
#   ifdef sparc
#      define WWFORXSUN
#   endif /* sparc */
#endif /* WWFORX */

#if defined(c_plusplus)||defined(__GNUG__)||defined(__GNUC__)
# define WWFULLFUNCTION	/* full function declaration prototypes accepted */
#endif

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif /* TRUE */
# define NULLPTR(x)  ((x *)0)  /* null (coerced) */
#define WWERRLEN  200  /* length of errmsg buffer */
#define WWNOCLIP  (-1) /* an uninteresting value to mark noclipbox out */
#define WWERROR   (-2) /* failure */
#define WWOK  0  /* ok */
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
 * info about a bitmap. sadly cannot forward reference the window typdef
 */
typedef struct _bitmap{
	box		bm_box;		/* initially box enclosing whole bitmap */
	int		bm_colours;	/* max num colours in bitmap (eg 2!) */
	struct _window	*bm_window;	/* point to window it is for, or null */
	int		*bm_junk;	/* more irrelevant info */
}bitmap;
/*
 * a cursor with hot spot and offset from usual cursor image
 */
typedef struct _cursor{
	bitmap	*c_bm;
	bitmap	*c_mask;
	int	c_rop;
	int	c_xhot,c_yhot;
	int	c_xoffset,c_yoffset;
}cursor;
/*
 * info about a window
 */
typedef struct _window{
	bitmap	*w_bm;		/* the bitmap that is the window */
	int	w_xrel,w_yrel;	/* relative posn of window to parent */
	box	w_fnbox;	/* posn of fnbox if to be used */
	int	w_flags;
	bitmap	*w_icon;	/* -> the icon or null */
	cursor	*w_cursor;	/* -> the cursor or null */
	int	*w_junk;	/* more irrelevant info */
}window;
/*
 * define w_flags bit settings
 */
#define WICONSEEN 01  /* the icon is visible */
#define WSEEN     02  /* window can be seen */
#define WINSIDE   04  /* cursor is inside */
#define WKILL     010 /* report if user wants/hasdestroyed window */
/*
 * define bmbox arguments that can be or-ed together
 */
#define BMCLEARALL  01  /* clear box, including edges */
#define BMCLEAR     02  /* clear inside of box, not including edges */
#define BMNOTALL    04  /* not all of box */
#define BMNOT       010 /* not inside of box */
#define BMEDGES     020 /* draw edges of box, using dd->d_line */
#define BMWIN     04000 /* use ddwin->w_bm instead of ddbm */
/*
 * define bmcopy arguments that can be or-ed together.
 */
#define BMGET		0100	/* get the returned bitmap (implies BMTO) */
#define BMFREE		0200	/* free the given bitmap (implies BMFROM) */
#define BMTO		01000	/* copy to the given bitmap */
#define BMFROM		02000	/* copy from the given bitmap */
/* and  BMWIN		04000	   use ddwin->w_bm instead of ddbm */
/*
 * define boxop arguments can be one of these
 */
#define BOXINTERSECT	1	/* return common box between two boxes. if none return 0,0,0,0 */
#define BOXCENTRE	2	/* return first box when centred in 2nd box, with reduced size if 2nd box smaller */
#define BOXENCLOSING	3	/* smallest box enclosing both boxes */
#define BOXREDUCE	4	/* reduce a to the size of b, unless is it smaller already */
#define BOXADD		5	/* offset first box by 2nd box top/left */
#define BOXBOUNCE	6	/* bounce first box off walls of 2nd, and reduce if necessary */
#define BOXBRLAP	7	/* 2nd box moved to overlap 1st as much as possible,
	(to bottom right) but leaving a border so that either can be on top & picked */
#define BOXBLLAP	8	/* overlap to bottom left */
#define BOXTLLAP	9	/* overlap to top left */
#define BOXTRLAP	10	/* overlap to top right */
/*
 * define corep copack arguments can be one of these
 */
#define COGET		1	/* get colour map entries */
#define COSET		2	/* set them */
#define COSETDEFAULT	4	/* set the default */
/*
 * info about a font.
 * To put a char on a base line at y=y, use ftxprint(,y+font.f_baseline,...)
 */
typedef struct _fontinfo{
	char		*f_name;		/* mallocd filename */
	int		f_height,f_width;	/* of biggest char */
	cursor		*f_cursor;		/* -> suitable text cursor, if any */
	struct _fontinfo *f_next;		/* next in circular list of fonts */
	int		f_baseline;		/* >=0 num pixels under baseline */
	int 		*f_junk;
	
}fontinfo;
/*
 * define ftprint arguments that can be or-ed together
 */
#define FTNOT		02	/* not box after putting in text */
#define FTOVER		04	/* dont clear box before use */
#define FTWIN		010	/* use ddwin->w_bm instead of ddbm */
#define FTVERT		020	/* centre text vertically in height */
#define FTHORIZ		040	/* centre text horizontally in width */
#define FTCENTRE	(FTVERT|FTHORIZ)	/* centre text */
#define FTRIGHT		0100	/* right justify */
#define FTBOTTOM	0200	/* bottom justify */
#define FTPROP		0400	/* use proportional spacing if available */
#define FTCONTROL	01000	/* show control chars as inverse video */
#define FTKERN		02000	/* only to ftbox: return exact box for string */
/*
 * all special info about editing chars to use
 */
typedef struct{
	char	mc_erase;
	char	mc_kill;
	char	mc_delword;
	char	mc_retype;
}magic;
/*
 * global info about the state of ww
 */
typedef struct _wwstate{
	int	d_line;		/* line style in use */
	int	d_rop;		/* raster op in use */
	int	d_buttons;	/* which mouse buttons down */
	int	d_x,d_y;	/* x and y position of mouse in pixels */
	magic	d_magic;	/* chars to use in standard editing */
	char	d_errmsg[WWERRLEN];	/* last error message */
	int	d_flags;	/* more about this state */

	int	d_event;	/* last input event giving this state */
	char	d_char;		/* char typed in */
	window	*d_newwindow;	/* window selected by mouse */
	int	d_select;	/* fds you can use in select before ipwait */
	int	d_fore;		/* foreground colour (for lines eg) */
	int	d_back;		/* background colour (eg for BMCLEAR) */
	int	d_colours;	/* default num colours in bitmaps and windows */
	int	(*d_ipwait)();	/* point to input wait filter */
	int	d_shift;	/* meta, shift and control key settings */
	long	*d_selectmore;	/* ->an fd_set of d_selectlength bits */
	int	d_selectlength;	/* d_selectmore length in bits */
	int	d_buttonmap[3];	/* what bit is set for a button: l,m,r */
}wwstate;
/*
 * define dd->d_buttons settings that can be or-ed together
 */
#define	ITEMBUTTON	01
#define MENUBUTTON	02
#define SHOWBUTTON	04
/*
 * define dd->d_buttonmap indexes in which to set ITEMBUTTON etc.
 */
#define BUTINDEXLEFT	0
#define BUTINDEXMIDDLE	1
#define BUTINDEXRIGHT	2
/*
 * define dd->d_shift settings that can be or-ed together (need ipset(IPRAW))
 */
#define IPSHIFT		01
#define IPCONTROL	02
#define IPMETA		04
/*
 * define dd->d_event can be one of these
 */
#define IPOTHER 	0	/* mouse position or button change */
#define	IPKEY		1	/* keyboard input in d_char */
#define IPSIZE		3	/* window size has changed */
#define	IPLEAVE		4	/* window not selected. guarantee buttons=0 */
#define	IPENTER		5	/* window selected. sets d_newwindow */
#define IPSEEN		6	/* window visible. icon may not be */
#define IPNOTSEEN	7	/* window no longer visible. icon may be */
#define IPSHIFTED	8	/* shift, control, meta change. Need IPRAW */
#define IPWANTKILL	9	/* user wants to kill window. Need WWREPORTKILL */
#define IPKILLED	10	/* user has killed window. Need WWREPORTKILL */

#define IPDESELECT	20	/* unhighlight the selection CAAG */
#define IPDELETE	21	/* delete the selection CAAG */
#define IPREPLACE	22	/* replace selection with wwshare CAAG */
#define IPINSERT	23	/* insert the wwshare at the selection CAAG */
#define IPACCEPT	24	/* insert the wwshare at the selection and select it CAAG */
#define IPJOIN		25	/* new subwindow has appeared CAAG */

#define SHARE_DELETE	((char *) 1)	/* wwshare should delete external selection CAAG */
#define SHARE_REPLACE	((char *) 2)	/* wwshare should replace external selection CAAG */
#define SHARE_INSERT	((char *) 3)	/* wwshare should insert at external selection CAAG */
#define SHARE_ACCEPT	((char *) 4)	/* wwshare should insert and select at external selection CAAG */
#define SHARE_KEYBOARD	((char *) 5)	/* wwshare should redirect keyboard input to selection holder CAAG */

/*
 * define dd->d_rop and raster operations values. WWNOT can be or-ed with one.
 * one of, but WWNOT can be ored in with any of the others.
 * they have slightly limited meaning for line drawing.
 */
#define WWCOPY		0	/* d <- s */
#define WWXOR		01	/* d <- d ^ s */
#define WWOR		02	/* d <- d | s */
#define WWAND		03	/* d <- d & s */
#define WWNOT		04	/* use ~s instead of s */
/*
 * define dd->d_fore and d_back colours can be (using ww bit-op colour map)
 */
#define	COBLACK		0
#define	CORED		01
#define	COGREEN		02
#define	COBLUE		04
#define	COYELLOW	03
#define	COCYAN		06
#define	COMAGENTA	05
#define	COWHITE		07
/*
 * external representation of ww objects
 */
typedef union{
	bitmap *ex_bitmap;
	cursor *ex_cursor;
	int	ex_grey;
}exrep;
/*
 * define exrep external representations
 */
#define EXBITMAP	0
#define EXCURSOR	1
#define EXGREY		2
/* 
 * define ipset arguments can be one of these
 */
#define IPON		1
#define IPOFF		2
#define IPSAMPLE	3
#define IPREQUEST	4
#define IPNOKEY		5	/* get keyboard input from parent win too */
#define IPRAW		6	/* shift, control and meta key transitions */ 
/*
 * define line routine arguments can be one of these
 */
#define LNDRAWABS 00
#define LNMOVEABS 01
#define LNDRAWREL 02
#define LNMOVEREL 03
/*
 * info about a text-entry-and-editing area
 */
typedef struct _txinfo{
	box		tx_box;		/* box where text is entered */
	char		*tx_start;	/* ->text string, null terminated */
	char  		*tx_next;	/* ->terminating null */
	char		*tx_term;	/* 0 or ->chars to cause txfollow to return */
	int		tx_length;	/* length of text, not inc extra null at end */
	int		tx_left;	/* current user selected char(s): left */
	int		tx_right;	/* and right (0=>left of first char of text) */
	int		tx_flags;
	int		tx_xmax;	/* max num columns in display */
	int		tx_ymax;	/* max num lines in display */
	int		tx_offset;	/* number of chars to start of display */
	struct _txinfo	*tx_group;	/* 0 or next in circle list of single selections */
	struct _txinfo	*(*tx_check)();	/* 0 or routine called when something about to happen */
	int		*tx_junk;	/* more irrelevant info */
}txinfo;
 /* define txvisible arguments can be one of these
 */
#define TXSET		1	/* set this txinfo visible */
#define TXCLEAR		2	/* set this txinfo not visible */
#define TXCLEARALL	3	/* set all this circular list not visible */
#define TXWHICH		4	/* return which txinfo in list is visible */

/*
 * info about an extension of the txinfo area that is used to hold a filename and where
 * you can wander around the filesystem
 */
typedef struct _treeinfo{
	box		tr_box;		/* the whole area to display in */
	box		tr_hitbox;	/* the sensitive area for popups */
	txinfo		*tr_tx;		/* the absolute or relative pathname in a txinfo area */
	int		tr_flags;
	struct _treeinfo *tr_base;	/* the current working dir for this filename */
}treeinfo;
/*
 * define treeinfo tr_flags bit settings
 */
#define TRABSOLUTE	01	/* use absolute pathnames */
#define TRCWDOK		02	/* this is the correct current dir */
/*
 * define stack routine arguments can be one of these
 */
#define WWPUSH		1	
#define WWPUSHOFF	2
#define WWPOP		3
#define WWSET		4
#define WWPUSHBUSY	5	/* custack */
#define WWPUSHON	6	
/*
 * define ddselect arguments can be one of these
 */
#define WWSELCLEAR	0
#define WWSELSET	1
#define WWSELTEST	2
/*
 * define ww*map arguments can be one of these
 */
#define WWMAPGET	01	/* copy bitmap to char array */
#define WWMAPPUT	02	/* copy char array to bitmap */
#define WWMAPOK		04	/* boundary errors are ok */
#define WWMAPTILE	010	/* tile from the source */
/* 
 * define wwxget arguments can be these ored together
 */
#define WWSIZEFROMUSER	01	/* get better size for window from user */
#define WWOPENICONISED	02	/* start window iconised */
#define WWINTERNALSIZE	04	/* size is of inside window, not frame!  */
#define WWREPORTKILL	010	/* report is user wants/has window killed */
/*
 * default pointers set by wwinit.
 */
extern wwstate	*dd;
extern window	*ddwin;
extern bitmap	*ddbm;
extern fontinfo	*ddfont;
extern box	 noclipbox;
extern treeinfo *treebase;
#ifdef WWFORPERQ2
# define void	int
#endif /* WWFORPERQ2 */
/*
 * External Typing: rest of file generated automatically. BEWARE
 */
#ifndef WWFULLFUNCTION
void	 bmbox();
void	 bmcircle();
box	 bmclip();
bitmap	*bmcopy();
void	 bmdraw();
int	 bmfill();
void	 bmfree();
bitmap	*bmget();
int	 bmgetclip();
void	 bmgrey();
void	 bmxbox();
void	 bmxcopy();
void	 bmxdraw();
bitmap	*bmxget();
box	 boxbuild();
int	 boxinside();
box	 boxop();
box	 boxzoom();
int	 copack();
int	 corep();
int	 cosize();
cursor	*cudecode();
char	*cuencode();
void	 cufree();
void	 cuset();
void	 custack();
void	 cuwin();
void	 cuxstack();
int	 ddselect();
int	 exread();
int	 exwrite();
box	 ftbox();
cursor	*ftcursor();
void	 ftfree();
fontinfo	*ftload();
void	 ftprint();
void	 ftstack();
void	 ftxprint();
void	 ipset();
int	 ipwait();
void	 line();
void	 wwexit();
void	 wwfree();
int	 wwinit();
unsigned	 wwintmap();
void	 wwnoise();
void	 wwpanic();
char	*wwshare();
window	*wwxget();
void	 wwxstack();
bitmap	*xxcut();
bitmap	*xxgrey();
void	 xxneutral();
int	 xxrelease();
#else /* WWFULLFUNCTION */

void	 bmbox(box b,int flags);
void	 bmcircle(bitmap *bm,int x,int y,int radius,int flags);
box	 bmclip(bitmap *bm,box b);
bitmap	*bmcopy(bitmap *bm,box b,int flags);
void	 bmdraw(int xa,int ya,int xb,int yb,bitmap *bm,int rop);
int	 bmfill(bitmap *bm,bitmap *tile,int x,int y);
void	 bmfree(bitmap *bm);
bitmap	*bmget(int width,int height);
int	 bmgetclip(bitmap *bm,box *b);
void	 bmgrey(box b,int sh);
void	 bmxbox(bitmap *bm,box b,int flags);
void	 bmxcopy(bitmap *from,box frombox,bitmap *to,box tobox,int rop);
void	 bmxdraw(int xa,int ya,int xb,int yb,bitmap *bm,int rop,int width);
bitmap	*bmxget(int width,int height,int colours);
box	 boxbuild(int left,int top,int right,int bottom);
int	 boxinside(box b,int x,int y);
box	 boxop(box a,box b,int flag);
box	 boxzoom(box b,int dist);
int	 copack(int rep,int colour,int flags);
int	 corep(bitmap *bm,unsigned char *r,unsigned char *g,unsigned char *b,int step,int start,int count,int flags);
int	 cosize(bitmap *bm,int colours);
cursor	*cudecode(char *pattern,int style);
char	*cuencode(cursor *cp,int style,int *length);
void	 cufree(cursor *cp);
void	 cuset(cursor *cp,window *wp,int flag);
void	 custack(cursor *cp,int flag);
void	 cuwin(cursor *cp,window *wp);
void	 cuxstack(window *win,cursor *cp,int flag);
int	 ddselect(int fd,int flag);
int	 exread(int file,int *style,exrep *ex);
int	 exwrite(int file,int what,int style,exrep ex);
box	 ftbox(fontinfo *fp,char *str,int len,int flags);
cursor	*ftcursor(int width,int height);
void	 ftfree(fontinfo *fp);
fontinfo	*ftload(char *file);
void	 ftprint(box b,char *str,int flags);
void	 ftstack(int flag);
void	 ftxprint(int x,int y,char *str,int len,int rop,fontinfo *font,bitmap *bm,int flags);
void	 ipset(int flag);
int	 ipwait();
void	 line(int x,int y,int flag);
void	 wwexit();
void	 wwfree(window *win);
int	 wwinit();
unsigned	 wwintmap(bitmap *bm,int x,int y,unsigned set,int flags);
void	 wwnoise();
void	 wwpanic(char *s);
char	*wwshare(char *what,int length,char *filename,int *returnlen);
window	*wwxget(box size,int colours,char *label,int flags);
void	 wwxstack(window *win,int flag);
bitmap	*xxcut(int fullscreen);
bitmap	*xxgrey(unsigned int intensity,unsigned int cache);
void	 xxneutral();
int	 xxrelease(box b);
#endif /* WWFULLFUNCTION */
#endif /* _WWINFO_H */
