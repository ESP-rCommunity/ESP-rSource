/*
  Common start-up code for ESP-r programs allowing terminal
  and file arguments to be passed to the Fortran `main' program
  from the invocation line.  All old main programs
  now become subroutines.
*/
#include <stdio.h>
#include <stdlib.h>

main(argc,argv)
int argc;
char** argv;
{
  long int term_type,linf; /* terminal type and lenght of file name */
  int i;
  char *mode = "graphic";  /* terminal mode string */
  char *inf  = "UNKNOWN";  /* input file string */

  term_type = 8;           /* initial assumptions */

/* 
   Get arguments from command line:
*/
  if(argc == 1 ) {                 /* no commands to put in defaults */
     term_type = 8;                /* if no argument set to 8 */
     linf = strlen(inf);
     initpf_(&term_type,inf,linf); /* call the fortran program */
  } else if (argc > 1 )  {
     for (i = 1; i < argc; i++) {
       if (strncmp(argv[i], "-help", 5) == 0) {
         printf(" \n");
         if (strstr(argv[0],"bps")) {
           printf(" This is the ESP-r Simulator. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <configuration file>]\n");
         } else if (strstr(argv[0],"cfd")) {
           printf(" This is the stand-alone CFD module. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <configuration file>]\n");
         } else if (strstr(argv[0],"cfg")) {
           printf(" This is the ESP-r Topology checker. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <configuration file>]\n");
         } else if (strstr(argv[0],"clm")) {
           printf(" This is the ESP-r Climate analyser. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <climate db file>]\n");
         } else if (strstr(argv[0],"grd")) {
           printf(" This is the ESP-r gridding module. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <configuration file>]\n");
         } else if (strstr(argv[0],"mfs")) {
           printf(" This is the stand-alone mass flow solver. \n");
           printf(" Use: [-mode {text|page|graphic|script}] \n");
         } else if (strstr(argv[0],"pdb")) {
           printf(" This is the ESP-r Plant database module. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <plant db file>]\n");
         } else if (strstr(argv[0],"pdf")) {
           printf(" This is the ESP-r Plant network description module. \n");
           printf(" Use: [-mode {text|page|graphic|script}] \n");
         } else if (strstr(argv[0],"plt")) {
           printf(" This is the stand-alone Plant solver. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <plant network file>]\n");
         } else if (strstr(argv[0],"pro")) {
           printf(" This is the ESP-r Event profiles database module. \n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <profiles db file>]\n");
         } else if (strstr(argv[0],"res")) {
           printf(" This is the ESP-r Results Analysis module.\n");
           printf(" Use: [-mode {text|page|graphic|script}] [-file <simulation results file>]\n");
         } else if (strstr(argv[0],"tdf")) {
           printf(" This is the Temporal database module.\n");
           printf(" Use: [-mode {text|page|graphic|script}] \n");
         } else  {
           printf(" Use: [-mode {text|page|graphic|script}] [-file <name>]\n");
         }
         printf(" \n");
         printf("   where `-mode' options are:\n") ;
         printf("     text    = text mode with no page breaks,\n");
         printf("     page    = text mode with page breaks,\n");
         printf("     graphic = graphic mode (default if mode omitted),\n");
         printf("     script  = text mode for redirection of output (should be used \n");
         printf("               in a Bourne shell).  Example: To run the Results Analysis \n");
         printf("               module the following command is typical: \n");
         printf("                   res -mode script -file xx.res 2>xx.table  \n");
         printf("               A similar line might be included within a script. \n");
         printf(" \n");
         printf(" -help :this help message.\n");
         printf(" \n");
         exit(1);
       }
       if (strncmp(argv[i], "-mode", 5) == 0) {
         mode = argv[++i];
         if (strncmp(mode, "text", 4) == 0)         term_type = -1; 
         else if (strncmp(mode, "page", 4) == 0)    term_type = -2; 
         else if (strncmp(mode, "graph", 5) == 0)   term_type = 8; 
         else if (strncmp(mode, "script", 6) == 0)  term_type = -6; 
         else term_type = 10;
       }
       if (strncmp(argv[i], "-file", 5) == 0) {
         if (argc > i+1 ) {
	   inf = argv[++i];
         } else {
           printf("Exit: -file no parameter supplied...\n");
           exit(0);
         }
       }
     }
     printf("Starting %s in %s mode with file %s \n",argv[0],mode,inf);
     linf = strlen(inf);
     initpf_(&term_type,inf,linf);   /* call the fortran program with file */
  }
}

MAIN_()
{
/*
   Dummy main to overcome problem when Fortran
   library does not already include a dummy main.
*/
}
