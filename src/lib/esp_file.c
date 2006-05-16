#include <gtk/gtk.h>
#include <esp-r.h>

void askf_(gchar *q1, gchar *file, int lenq1, int lenfile)
{
   GtkWidget *filew;
   gchar *q1_local;
   gchar *file_local;
   gint result;
        
   q1_local = g_strndup(q1, (gsize) lenq1);
   file_local = g_strndup(file, (gsize) lenfile);
   
   /* 
   Create a new file selection widget without std directory buttons.
   */
   filew = gtk_file_selection_new (q1_local);
   gtk_file_selection_hide_fileop_buttons(GTK_FILE_SELECTION(filew));
   
   /* 
   Lets set the displayed filename to the one passed.
   */
   gtk_file_selection_set_filename (GTK_FILE_SELECTION(filew), file_local);
       
   /* 
   Set dialog properties and wait for user response.
   */
   gtk_window_set_modal (GTK_WINDOW (filew), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (filew), GTK_WINDOW (window));

   result = gtk_dialog_run (GTK_DIALOG (filew));
   switch (result)
      {
       case GTK_RESPONSE_OK:
       /* 
       Need to terminate this string properly for return to fortran.
       compiler complains: assignment discards qualifiers from pointer target type
       */
          file_local = gtk_file_selection_get_filename (GTK_FILE_SELECTION(filew));
          g_stpcpy (file,file_local);
          break;
       case GTK_RESPONSE_CANCEL:
          file_local = "CANCEL";
          g_stpcpy (file,file_local);
          break;
       default:
          break;
      }
      
    gtk_widget_destroy (filew);
   
}
