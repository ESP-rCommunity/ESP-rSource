/*
esru_util.c	(independant of graphics) 
These routines are called from ESP-r fortran code. The routines are :-
   f_to_c_l() confirm Fortran string length
   ckaccess_() finds access parameters of a file
   getfilelist_() return list of files in a folder
   wwcopen_() open ww commands output file 
   wwcclose_() Close and mark end or ww commands file
   wwcsetstart_() indicate start of a set of drawing commands
   wwcsetend_()	 indicate end of a set of drawing commands
   curproject_() pass in info on the current project from fortran
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/time.h>
#include <string.h>
#include <fc_commons.h>
/* external defines are found in wwxlegacy.h */
#include "wwxlegacy.h"

char cfgroot[25];	/* f77 project root name    */
char path[73];	/* f77 project path    */
char upath[73];	/* f77 users path    */
char imgpth[25];	/* f77 relative path to images    */
char docpth[25];	/* f77 relative path to documents    */
char tmppth[25];	/* f77 relative path to scratch folder    */

/* the wwc_ok and wwc_macro are treated as extern types in esru_x.c */
int  wwc_ok = 0;   /* assume no echo of drawing commands to wwc */
int  wwc_macro = 0;   /* assume not in a macro drawing command (etplot) */
int browse;	/* if = 0 then user owns, if = 1 user browsing */
FILE *wwc;

/* ************** Confirm Fortran string length *************** */
/*
 Since the string length automaticly passed between Fortran and C
 tends to represent the "defined" string length rather than the actual
 string length here is a bit of code to start at the "defined" end
 and work backwards to find the last non-blank character position.
*/
void f_to_c_l(msg,f_len,len)
  char    *msg;         /* character string */
  int     *f_len,*len;  /* fortran string length,
                           found position of last non blank character */
{
  int lm, sl, n, found;       /* local string lengths found by test  */
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

/* ****** ckaccess_ ********* */
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
 char name2[140];  /* allow sufficient buffer size for long file names */
 struct stat st;

/* Work with copy of file name. */
  f_to_c_l(fname,&len,&ilen);
  if ( ilen > len ) ilen = len;  /* in case ilen is corrupt, set to len */
  strncpy(name2,fname,(unsigned int)ilen); name2[ilen] = '\0';
/* debug fprintf(stderr,"file is %s %d %d \n",name2,ilen,len); */
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
    fprintf(stderr,"unable to get stats on %s\n",name2);
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
        `bsm` get list of BASESIMP files,
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
    strncpy(file_list[i],"                                                                         ",73);
  }
/* Work with copy of folder name. */
  f_to_c_l(folder,&lenfolder,&ilen); strncpy(name2,folder,(unsigned int)ilen); name2[ilen] = '\0';
  f_to_c_l(act,&lenact,&ialen); strncpy(act2,act,(unsigned int)ialen); act2[ialen] = '\0';
  i = stat(name2, &st);
  ifolder = (st.st_mode&0x4000); /* returns nonzero if a folder */
  if (ifolder == 0) {
    fprintf(stderr,"folder name passed %s is not recognized as a folder.\n",name2);
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
      } else if(strcmp("bsm",act2)== 0) { /* If request for BASESIMP files only, then build this list. */
        if (strstr(dirt->d_name,".bs,")) foundone = 1;
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
    strncpy(locflist,"                                                                         ",73);
  for(num = 0; num < locnflist; num++) {	/* for each recovered string...  */
    strncpy(&locflist[ipos],file_list[num],(unsigned int)lenflist);	/* copy to local array */
    ipos=ipos+lenflist;
  }
  *flist = *locflist;	/* copy locflist back to flist */
  *nflist = locnflist;
  return;
}

/*  open ww commands output file */
void wwcopen_(name,len)
char *name;
int len;	/* string length from fortran */
{
 int ilen;
 char name2[140];
 wwc_ok = 1;   /* set flag to echo drawing commands to wwc */

/*
 * Terminate at fortran length, find actual string length
 * and then reterminate.
 */
  f_to_c_l(name,&len,&ilen); strncpy(name2,name,(unsigned int)ilen); name2[ilen]='\0';
  if ((wwc = fopen(name2,"w"))==NULL) {
    fprintf(stderr,"could not open wwc file %s\n",name2);
    exit(1);
  }
  return;
}

/* Close and mark endww commands file if one has been setup */
void wwcclose_(name,len)
char *name;
int len;	/* string length from fortran */
{
 int ilen;
 char name2[140];
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
  iincomp,iincon,len_root,len_fpath,len_fupath,len_fimgpth,len_fdocpth,len_ftmppth)
  char *fcfgroot;	/* f77 project root name    */
  char *fpath;	/* f77 project path    */
  char *fupath;	/* f77 users path    */
  char *fimgpth;	/* f77 relative path to images    */
  char *fdocpth;	/* f77 relative path to documents    */
  char *ftmppth;	/* f77 relative path to scratch folder    */
  long int *ibrowse;	/* if = 0 then user owns, if = 1 user browsing */
  long int *iincomp;	/* current number of zones in model */
  long int *iincon;	/* current number of connections in model */
  int  len_root,len_fpath,len_fupath,len_fimgpth,len_fdocpth,len_ftmppth;	/* length of strings from f77  */
{
  int  l_root,l_fpath,l_fupath,l_fimgpth,l_fdocpth,l_ftmppth;

  l_root = l_fpath = l_fupath = l_fimgpth = l_fdocpth = l_ftmppth =0;
  browse = (int) *ibrowse;
  strncpy(cfgroot,"                        ",24);
  f_to_c_l(fcfgroot,&len_root,&l_root); strncpy(cfgroot,fcfgroot,(unsigned int)l_root);	/* copy to static */
  cfgroot[l_root] = '\0';
  strncpy(imgpth, "                        ",24);
  f_to_c_l(fimgpth,&len_fimgpth,&l_fimgpth); strncpy(imgpth,fimgpth,(unsigned int)l_fimgpth);	/* copy to static */
  imgpth[l_fimgpth] = '\0';
  strncpy(docpth, "                        ",24);
  f_to_c_l(fdocpth,&len_fdocpth,&l_fdocpth); strncpy(docpth,fdocpth,(unsigned int)l_fdocpth);	/* copy to static */
  docpth[l_fdocpth] = '\0';
  strncpy(tmppth, "                        ",24);
  f_to_c_l(ftmppth,&len_ftmppth,&l_ftmppth); strncpy(tmppth,ftmppth,(unsigned int)l_ftmppth);	/* copy to static */
  tmppth[l_ftmppth] = '\0';
  strncpy(path, "                                                                         ",72);
  f_to_c_l(fpath,&len_fpath,&l_fpath); strncpy(path,fpath,(unsigned int)l_fpath);	/* copy to static */
  path[l_fpath] = '\0';
  strncpy(upath,"                                                                         ",72);
  f_to_c_l(fupath,&len_fupath,&l_fupath); strncpy(upath,fupath,(unsigned int)l_fupath);	/* copy to static */
  upath[l_fupath] = '\0';
  c1_.NCOMP = *iincomp;  // pass curent number of zones and connections to c1_ structure
  c1_.NCON = *iincon;    // needed to ensure 32 bit and 64 bit safe transfer between fortran and c
/* debug  fprintf(stderr,"cfgroot %s\n",cfgroot);  */
/* debug  fprintf(stderr,"imgpth %s\n",imgpth);  */
/* debug  fprintf(stderr,"docpth %s\n",docpth);  */ 
/* debug  fprintf(stderr,"path %s\n",path);  */
/* debug  fprintf(stderr,"upath %s\n",upath);  */
/* debug  fprintf(stderr,"browse %d\n",browse);  */
/* debug  fprintf(stderr,"ibrowse %ld\n",*ibrowse); */
/* debug  fprintf(stderr,"iincompb %d\n",c1_.NCOMP); */
/* debug fprintf(stderr,"iincon %d\n",c1_.NCON); */
  return;
}

// good place for other functions to pass information from fortran to c (like image_ and ray2_)

/* curviews_() - pass in info on the current views from fortran */
void curviews_(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,JITOBS,
     JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW)
  float *EVX,*EVY,*EVZ;  // eye point X Y Z
  float *VX,*VY,*VZ;     // viewed point X Y Z
  float *EAN,*DIS;       // angle of view and distance
  long int *JITZNM;  //zone name toggle: display = 0, hidden = 1
  long int *JITSNM;  // surface name toggle: display = 0, hidden = 1
  long int *JITVNO;  // vertex toggle: display = 0, hidden = 1
  long int *JITOBS;  // obstruction toggle: 
  long int *JITSNR;  // surf normal toggle: display = 0, hidden = 1
  long int *JITGRD;  // grid toggle: display = 0, hidden = 1
  long int *JITORG;  // origin toggle: display = 0, hidden = 1
  long int *JITBND;  // bounds toggle: static = 0, optimum = 1, zone focus = 2
  long int *JITDSP;  // labels toggle: all surf + obs = 0, all surf = 1, partn = 2,
                     // similar = 4, surfs + obs+ ground = 5, ground only = 6
  long int *JITHLS;  // highlight toggle: normal 0, constr 1, trans/opaq 2, part atrib 3
  long int *JITHLZ;  // 2nd hilight attribute
  long int *JITPPSW; // current view - perspective/plan/south/west
{
  ray2_.ITDSP = *JITDSP;
  ray2_.ITBND = *JITBND;
  ray2_.ITZNM = *JITZNM;
  ray2_.ITSNM = *JITSNM;
  ray2_.ITVNO = *JITVNO;
  ray2_.ITORG = *JITORG;
  ray2_.ITSNR = *JITSNR;
  ray2_.ITOBS = *JITOBS;
  ray2_.ITHLS = *JITHLS;
  ray2_.ITHLZ = *JITHLZ;
  ray2_.ITGRD = *JITGRD;
  ray2_.GRDIS = *DIS;
  ray2_.ITPPSW = *JITPPSW;
  image_.EYEM[0] = *EVX;
  image_.EYEM[1] = *EVY;
  image_.EYEM[2] = *EVZ;
  image_.VIEWM[0] = *VX;
  image_.VIEWM[1] = *VY;
  image_.VIEWM[2] = *VZ;
  image_.ANG = *EAN;
}

/* pushgzonpik_() pass info on gzonpik common to C code from fortran. */
void pushgzonpik_(jizgfoc,jnzg)
  long int *jizgfoc;  // current index of focus zone
  long int *jnzg;     // number of selected zones
{
  gzonpik_.izgfoc = *jizgfoc;
  gzonpik_.nzg = *jnzg;
}

/* pushnznog_() pass one item of nznog array to C code from fortran. */
void pushnznog_(jnznog,jnznogv)
  long int *jnznog;  // index of the item (fortran counting)
  long int *jnznogv; // value of the item
{
  int item;
  item = (int)*jnznog-1;  // decrement index for c use
  gzonpik_.nznog[item]= *jnznogv;
}

