#include <stdio.h>
int redir_console_output_to_file_(char cMsg[], int iMsg_len)
{
      FILE *fout;
      
      fout = fopen ( "console_out.txt", "a" ) ;
      fprintf (fout, " %-1s \n", cMsg) ;
      fclose ( fout ) ;
      
      return 0;
}