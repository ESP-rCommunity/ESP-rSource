#include <stdio.h>

/* 
 cstripc strips comments from a ASCII file string and returns the data.
 It assumes that if a string begins with a '#' then the whole line is 
 a comment an the next line is read.  If a ' #' is discovered within
 a line the rest of the line is removed. 
 cstripc is the "c" equivalent of stripc in libesru_low.f.
 IER=0 if ok. MSG is a text string used in error messages.
 IEXP is the number of expected items in the line: 
   IEXP = 0 means don't care or already know no. items - don't check
   IEXP >0  means a specific number of items expected (error if not)
   IEXP = 99 check number of items and return in ITEMS

 Example of use: open a file (name) and scan it for specific tokens
 . . . 
 #include "wwinfo.h"
 FILE *wwc;
 int iexp, itm, ier;
 int ilen, count;
 char outstr[124];
 char word[72];
 . . .
 if ((wwc = fopen(name,"r"))==NULL) {
  fprintf(stderr,"could not open wwc file %s\n",name);
  return;
 }
 fprintf(stderr,"file is %s\n",name);
 do {	/* Read a line from file, skipping & stripping comments */
   iexp = 99; itm = 0; ier = 0;
   cstripc(wwc,outstr,&iexp,&itm,"wwc problem",&ier);

   count = sscanf(outstr,"%s",word);
   iexp = 99; itm = 0; ier = 0;
   if (strncmp(word, "*start_set", 10) == 0) {
   } else if (strncmp(word, "*end_set", 8) == 0) {
   } else if (strncmp(word, "*end_wwc", 8) == 0) {
       fprintf(stderr,"finished set of commands\n");
       fclose(wwc);
       return;
   } else if (strncmp(word, "*axiscale", 9) == 0) {
       iexp = 11;
       cstripc(wwc,outstr,&iexp,&itm,"axiscale cmd",&ier);
       sscanf(outstr,"%d%d%f%f%f%f%f%f%f%f%f",
         &gw,&gh,&xmn,&xmx,&ymn,&ymx,&xsc,&ysc,&sca,&xadd,&yadd);
       axiscale_(&gw,&gh,&xmn,&xmx,&ymn,&ymx,&xsc,&ysc,&sca,&xadd,&yadd);
   } else if (strncmp(word, "*edline", 7) == 0) {
   . . .
  } while ( ier == 0);
  fclose(wwc);
 
*/
cstripc(fp,outstr,iexp,items,msg,ier)
 char *msg;
 char outstr[124];
 int *iexp, *items, *ier;
 FILE *fp; 
{
  char wd[72], outs[124];
  int count = 0;
  int i,j;
/*
 * Read a line of the file, strip off any trailing blanks, if the first
 * character is a # then read the next line from the file.
 */
  *ier=0;
  again:
  fgets(outs,124,fp);
  i = strlen(outs);
  outs[i] = '\0';

  j = strcspn(outs,"#");	/* find if there is a # in the line */
  if (j == 0) goto again;   	/* commented line read another one */
  outs[j-1] = '\0';

  strcpy(outstr,outs);	/* copy to outstr param */

  if ( *iexp == 99) {		/* words in outs via excess string scan */
    count = sscanf(outs,"%s %s %s %s %s %s %s %s %s %s %s %s %s %s",
                         wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd);
    *items = count;
  } else if ( *iexp == 0) {
    *items = 0;
  } else if ( *iexp > 0) {
    count = sscanf(outs,"%s %s %s %s %s %s %s %s %s %s %s %s %s %s",
                         wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd,wd);
    if ( count != *iexp) {
      fprintf(stderr," Looking for %d items in %s\n",iexp,outs); /* warning message  */
      fprintf(stderr," %s\n",msg);
      *items = count; *ier = 1; return;
    } else {
      *items = count;
    }
  }
}  /* cstripc */
/*
* cgetw gets first WORD after position K from the STRING of
* characters. Words are separated by blanks, commas, or tabs.  Provides a warning
* message if ACT='W', a failure message if ACT='F' and  no message if ACT='-'.
*/
cgetw(instr,k,word,act,msg,ier)
 char *msg, *word;
 char instr[124];
 char act;
 int *k, *ier;
{
  int i,ils,ik,nk;
  int ij = 0;
  int iss = 0;		/* iss = spaces between position k and start of word  */

  ils = strlen(instr);
  instr[ils] = '\0';	/* make certain input string is terminated */
  ik = *k;

  if (instr==NULL ||*instr==NULL || ik >= ils) {  /* don't bother if instr blank  */
    *ier=1; return;				  /* or if position past end */
  }

  word[0] = '\0';	/* Extract the substring, clear word first. */
  for (i=ik; i<ils; i++) {
    if (isspace(instr[i]) && ij == 0) {	
      iss++;			/* If initial spaces keep track of how many */
    } else if (isspace(instr[i]) && ij > 0) {
      break;			/* if find a subsequent whitespace stop */
    } else if (!isspace(instr[i])) {
      word[ij] = instr[i];	/* Copy across non-whitespace */
      ij++;
    }
  }
  word[ij] = '\0';	/* terminate the word */

  nk = strlen(word);
  *k = ik + nk + iss;   /* initial position + length of word + skipped spaces */

  if (word==NULL || *word==NULL) {	/* error messages */
    if ( act =='W') {
      fprintf(stderr," WARNING Couldn`t get %s word in: %s\n",msg,instr);
    } else if ( act =='F') {
      fprintf(stderr," FAILURE Couldn`t get %s word in: %s\n",msg,instr);
    }
    *ier=1;
    return;
  }
}

/*
 * cgetwi gets first word after position K from "instr"
 * and converts it into an integer IV, tests it against
 * the minimum MN and the maximum MX and provides a warning
 * message if ACT='W', a failure message if ACT='F' and does
 * no range checking if ACT='-'. Words may be separated by blanks, 
 * commas, or tabs.
 */
cgetwi(instr,k,iv,mn,mx,act,msg,ier)
 char *msg;
 char instr[124];
 char act;
 int *k,*iv,*mn,*mx, *ier;
{
  char word[20];	/* sub-string to convert into a value */
  int ils,ik,imn,imx,iier;

  ik = *k; imn = *mn;  imx = *mx; iier = *ier;
  ils = strlen(instr);		/* Pick up length of incomming string. */
  instr[ils] = '\0';

  word[0] = '\0';	/* Extract the substring, clear word first. */
/*  strcpy(word," ");  */
  cgetw(instr,&ik,word,'-',"integer",&iier);	/* get the word from instr */
  if (iier != 0) {
    *ier = iier; return;
  }
  *iv = atoi(word);	/* convert to integer */
  *k = ik;		/* pass back positon of string after the word.  */

/* If range checking disabled, jump out. */
  if (act =='-') return;

  if ( *iv < imn ) {	/* Check iv against minimum and respond based on act. */
    if ( act =='W') {
      fprintf(stderr," WARNING in: %s\n",instr); /* warning message  */
      fprintf(stderr," the %s value %d < normal minimum %d !\n",msg,iv,imn); 
    } else if ( act =='F') {
      fprintf(stderr," FAILURE in: %s\n",instr); /* warning message  */
      fprintf(stderr," the %s value %d < allowable minimum %d !\n",msg,iv,imn); 
      *ier=1;
      return;
    }
  } else if ( *iv > imx) {	/* Check iv against max and respond based on act. */
    if ( act =='W') {
      fprintf(stderr," WARNING in: %s\n",instr); /* warning message  */
      fprintf(stderr," the %s value %d > normal mamimum %d !\n",msg,iv,imx); 
    } else if ( act =='F') {
      fprintf(stderr," FAILURE in: %s\n",instr); /* warning message  */
      fprintf(stderr," the %s value %d < allowable maximum %d !\n",msg,iv,imx); 
      *ier=1;
      return;
    }
  }
}


