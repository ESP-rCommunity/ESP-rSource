/* Functions to ask for user input. 
   
   Functions:
      askdialog - prompt for a text entry (with help, default and cancel options).
      askdialogcmd - prompt for a text entry (with help, default, cancel, alternative cmd options).
      askdialog2cmd - prompt for a text entry (with help, default, cancel, two alternative cmd options).
      askreal - prompt for a real number entry (with help, default and cancel options).
      askint - prompt for a integer entry (with help, default and cancel options). <<not here yet>>
     
*/
   
#include <stdio.h>
#include <gtk/gtk.h>
#include <glib.h>
#include <esp-r.h>

/* ***** askdialog_() ask for text input */
/* Could pass in:
    title for the pop up box
*/
void askdialog_(char *q1, char *reply,long int *ier, int lenq1, int lenrep)
/* q1 is the question being posed to the user
   reply is the reply but has the current value from the fortran side
   ier tracks if the default or cancel buttons have been selected (requires
   suitable actions on the fortran side).
*/
{
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;
   gchar *question_local;
   gint result;
   int no_valid_event;
   int lnq1;	/* for non-blank length */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   
   *ier=0;	/* Reset value of ier flag */
    
   reply_local = g_strndup(reply, (gsize) lenrep);
/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);

/* debug */ fprintf(stderr,"phrase %s\n",question_local);
/* debug */ fprintf(stderr,"text is %s\n",reply_local);
/* debug */ fprintf(stderr,"nb of help lines %d\n",help_lines);
    
   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Text request",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_entry_new ();
   
   /* Define entry box properties */
   gtk_label_set_line_wrap(GTK_LABEL (label), TRUE);
   gtk_entry_set_max_length (GTK_ENTRY (entry), lenrep);
   gtk_entry_set_text (GTK_ENTRY (entry), reply_local);
   gtk_dialog_set_default_response (GTK_DIALOG (askbox), GTK_RESPONSE_OK);

   
   /* Pack widgets and display */
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),label);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),entry);
   gtk_widget_show_all (askbox);
                      
   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   /* Run this as a while loop 'no_valid-event' so we can call other widgets, for 
      example, a help dialog directly from here */
   no_valid_event = TRUE;
   while ( no_valid_event) {
     result = gtk_dialog_run (GTK_DIALOG (askbox));
     switch (result) {
       case GTK_RESPONSE_OK:
       /* 
       Terminate this string properly for return to fortran 
       */
         reply_local = g_strstrip(gtk_editable_get_chars(GTK_EDITABLE (entry),0,-1));
         g_stpcpy (reply,reply_local);
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       case 99:
         *ier=-2;
         no_valid_event = FALSE;
         break;
       default:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       }
     }
   gtk_widget_destroy (askbox);
   
}

/* ***** askdialogcmd_() ask for text input with an additional command option */
/* Could pass in:
    title for the pop up box
*/
void askdialogcmd_(char *q1, char *reply, char *cmd, long int *ier, int lenq1, int lenrep, int lencmd)
/* q1 is the question being posed to the user
   reply is the reply but has the current value from the fortran side
   cmd is an alternative command string from the fortran side
   ier tracks if the default, cmd option or cancel buttons have been selected (requires
   suitable actions on the fortran side).
   lenq1 is passed length of prompt, lenrep is passed length of reply, lencmd is passed length of cmd.
*/
{
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;
   gchar *question_local;
   gchar *cmd_local;
   gint result;
   int no_valid_event;
   int lnq1,lncmd1;	/* for non-blank lengths */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   
   *ier=0;	/* Reset value of ier flag */
    
   reply_local = g_strndup(reply, (gsize) lenrep);
/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);
   lncmd1 = 0;
   f_to_c_l(cmd,&lencmd,&lncmd1);
   cmd_local = g_strndup(cmd, (gsize) lncmd1);

/* debug */ fprintf(stderr,"phrase %s\n",question_local);
/* debug */ fprintf(stderr,"text is %s\n",reply_local);
/* debug */ fprintf(stderr,"cmd is %s\n",cmd_local);
/* debug */ fprintf(stderr,"nb of help lines %d\n",help_lines);
    
   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Text request with option",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,cmd_local,98,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_entry_new ();
   
   /* Define entry box properties */
   gtk_label_set_line_wrap(GTK_LABEL (label), TRUE);
   gtk_entry_set_max_length (GTK_ENTRY (entry), lenrep);
   gtk_entry_set_text (GTK_ENTRY (entry), reply_local);
   gtk_dialog_set_default_response (GTK_DIALOG (askbox), GTK_RESPONSE_OK);

   
   /* Pack widgets and display */
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),label);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),entry);
   gtk_widget_show_all (askbox);
                      
   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   /* Run this as a while loop 'no_valid-event' so we can call other widgets, for 
      example, a help dialog directly from here */
   no_valid_event = TRUE;
   while ( no_valid_event) {
     result = gtk_dialog_run (GTK_DIALOG (askbox));
     switch (result) {
       case GTK_RESPONSE_OK:
       /* 
       Terminate this string properly for return to fortran 
       */
         reply_local = g_strstrip(gtk_editable_get_chars(GTK_EDITABLE (entry),0,-1));
         g_stpcpy (reply,reply_local);
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       case 98:
         *ier=2;
         no_valid_event = FALSE;
         break;
       case 99:
         *ier=-2;
         no_valid_event = FALSE;
         break;
       default:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       }
     }
   gtk_widget_destroy (askbox);
   
}

/* ***** askdialog2cmd_() ask for text input with two command options */
/* Could pass in:
    title for the pop up box
*/
void askdialog2cmd_(char *q1, char *reply, char *cmd1, char *cmd2, long int *ier, int lenq1, int lenrep, int lencmd1,int lencmd2)
/* q1 is the question being posed to the user
   reply is the reply but has the current value from the fortran side
   cmd is an alternative command string from the fortran side
   ier tracks if the default, cmd option or cancel buttons have been selected (requires
   suitable actions on the fortran side).
   lenq1 is passed length of prompt, lenrep is passed length of reply, lencmd is passed length of cmd.
*/
{
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;
   gchar *question_local;
   gchar *cmd1_local;
   gchar *cmd2_local;
   gint result;
   int no_valid_event;
   int lnq1,lncmd1,lncmd2;	/* for non-blank lengths */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   
   *ier=0;	/* Reset value of ier flag */
    
   reply_local = g_strndup(reply, (gsize) lenrep);
/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);
   lncmd1 = 0;
   f_to_c_l(cmd1,&lencmd1,&lncmd1);
   cmd1_local = g_strndup(cmd1, (gsize) lncmd1);
   lncmd2 = 0;
   f_to_c_l(cmd2,&lencmd2,&lncmd2);
   cmd2_local = g_strndup(cmd2, (gsize) lncmd2);

/* debug */ fprintf(stderr,"phrase %s\n",question_local);
/* debug */ fprintf(stderr,"text is %s\n",reply_local);
/* debug */ fprintf(stderr,"cmd1 is %s\n",cmd1_local);
/* debug */ fprintf(stderr,"cmd2 is %s\n",cmd2_local);
/* debug */ fprintf(stderr,"nb of help lines %d\n",help_lines);
    
   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Text request with option",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,cmd1_local,97,cmd2_local,98,
     "Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_entry_new ();
   
   /* Define entry box properties */
   gtk_label_set_line_wrap(GTK_LABEL (label), TRUE);
   gtk_entry_set_max_length (GTK_ENTRY (entry), lenrep);
   gtk_entry_set_text (GTK_ENTRY (entry), reply_local);
   gtk_dialog_set_default_response (GTK_DIALOG (askbox), GTK_RESPONSE_OK);

   
   /* Pack widgets and display */
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),label);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),entry);
   gtk_widget_show_all (askbox);
                      
   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   /* Run this as a while loop 'no_valid-event' so we can call other widgets, for 
      example, a help dialog directly from here */
   no_valid_event = TRUE;
   while ( no_valid_event) {
     result = gtk_dialog_run (GTK_DIALOG (askbox));
     switch (result) {
       case GTK_RESPONSE_OK:
       /* 
       Terminate this string properly for return to fortran 
       */
         reply_local = g_strstrip(gtk_editable_get_chars(GTK_EDITABLE (entry),0,-1));
         g_stpcpy (reply,reply_local);
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       case 97:	/* the first alternative command */
         *ier=2;
         no_valid_event = FALSE;
         break;
       case 98:	/* the second alternative command */
         *ier=3;
         no_valid_event = FALSE;
         break;
       case 99:
         *ier=-2;
         no_valid_event = FALSE;
         break;
       default:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       }
     }
   gtk_widget_destroy (askbox);
   
}

/* ***** askreal_() ask for real number input */
/* Could pass in:
    min and max values if 'F' failure flag set (this would 
      trap the error here.
    number of decimal places to display.
    title for the pop up box.
   Fortran side needs updated to account for returned values of IER.
*/
   
void askreal_(char *q1, float *reply,long int *ier, int lenq1)
/* q1 and q2 strings form the questioon being posed to the user
   reply is the reply but has the current value from the fortran side
   ier tracks if the default or cancel buttons have been selected (requires
     suitable actions on the fortran side).
*/
{
   GtkWidget *askbox, *entry, *label, *spinner;
   GtkAdjustment *spinner_adj;
   gchar *question_local;
   gint result;
   int no_valid_event;
   int lnq1;	/* for non-blank length */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   gdouble value;
   
   /* Reset value of ier flag */
   *ier=0;
   value = (double) *reply;

/* find out actual length of prompt. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);
/* debug */ fprintf(stderr,"r phrase %s\n",question_local);
/* debug */ fprintf(stderr,"r nb of help lines %d\n",help_lines);
        
   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Real number request",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_entry_new ();
   
   /* Define entry box properties */
   gtk_dialog_set_default_response (GTK_DIALOG (askbox), GTK_RESPONSE_OK);

   spinner_adj = (GtkAdjustment *) gtk_adjustment_new (value, -G_MAXFLOAT, G_MAXFLOAT, 0.1, 10., 10.);
      
   /* creates the spinner, with three decimal places */
   spinner = gtk_spin_button_new (spinner_adj, 0.1, 3);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),label);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),spinner);
   gtk_widget_show_all (askbox);
                      
   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   /* Run this as a while loop 'no_valid-event' so we can call other widgets, for 
      example, a help dialog directly from here */
   no_valid_event = TRUE;
   while ( no_valid_event) {
     result = gtk_dialog_run (GTK_DIALOG (askbox));
     switch (result) {
       case GTK_RESPONSE_OK:
       /* 
       Terminate this string properly for return to fortran 
       */
         value = gtk_adjustment_get_value(spinner_adj);
         *reply = (gfloat) value;
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       case 99:
         *ier=-2;
         no_valid_event = FALSE;
         break;
       default:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       }
     }
   gtk_widget_destroy (askbox);
}

/* ***** askint_() ask for number input */
/* Could pass in:
    min and max values if 'F' failure flag set (this would 
      trap the error here.
    title for the pop up box.
   Fortran side needs updated to account for returned values of IER.
*/
   
void askint_(char *q1, long int *reply,long int *ier, int lenq1)
/* q1 and q2 strings form the question being posed to the user
   reply is the reply but has the current value from the fortran side
   ier tracks if the default or cancel buttons have been selected (requires
     suitable actions on the fortran side).
*/
{
   GtkWidget *askbox, *entry, *label, *spinner;
   GtkAdjustment *spinner_adj;
   gchar *question_local;
   gchar *reply_local;
   gint result;
   int no_valid_event;
   int lnq1;	/* for non-blank lengths */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   gdouble value;
   
   /* Reset value of ier flag and cast integer reply to local double variable */
   *ier=0;
   value = (double) *reply;

/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);
/* debug */ fprintf(stderr,"i phrase %s\n",question_local);
/* debug */ fprintf(stderr,"i nb of help lines %d\n",help_lines);
        
   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Integer request",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_entry_new ();
   
   /* Define entry box properties */
   gtk_dialog_set_default_response (GTK_DIALOG (askbox), GTK_RESPONSE_OK);

   spinner_adj = (GtkAdjustment *) gtk_adjustment_new (value, -G_MAXFLOAT, G_MAXFLOAT, 1., 10., 10.);
      
   /* creates the spinner, with three decimal places */
   spinner = gtk_spin_button_new (spinner_adj, 1., 0);
   gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),label);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),spinner);
   gtk_widget_show_all (askbox);
                      
   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   /* Run this as a while loop 'no_valid-event' so we can call other widgets, for 
      example, a help dialog directly from here */
   no_valid_event = TRUE;
   while ( no_valid_event) {
     result = gtk_dialog_run (GTK_DIALOG (askbox));
     switch (result) {
       case GTK_RESPONSE_OK:
       /* 
       Terminate this string properly for return to fortran 
       */
         value = gtk_adjustment_get_value(spinner_adj);
         *reply = (gint) value;
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       case 99:
         *ier=-2;
         no_valid_event = FALSE;
         break;
       default:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       }
     }
   gtk_widget_destroy (askbox);
}

/* ***** askdialog248_() ask for multi-line text input */
/* Could pass in:
    title for the pop up box
*/
void askdialog248_(char *q1, char *reply,long int *ier, int lenq1, int lenrep)
/* q1 is the question being posed to the user
   reply is the reply but has the current value from the fortran side
   ier tracks if the default or cancel buttons have been selected (requires
   suitable actions on the fortran side).
*/
{
   GtkTextBuffer *buffer;
   GtkTextIter start;
   GtkTextIter end;
   GtkTextMark *end_mark;
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;
   gchar *question_local;
   gint result;
   int no_valid_event;
   int lnq1;	/* for non-blank length */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   
   *ier=0;	/* Reset value of ier flag */
    
   reply_local = g_strndup(reply, (gsize) lenrep);
/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);

/* debugfprintf(stderr,"phrase %s\n",question_local); */ 
/* debug fprintf(stderr,"text is %s\n",reply_local); */
/* debug fprintf(stderr,"nb of help lines %d\n",help_lines) */;
    
   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Text request",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_text_view_new ();  /* create a multi-line text editing area */
   gtk_text_view_set_wrap_mode ( GTK_TEXT_VIEW (entry), GTK_WRAP_WORD );       /* warp the words */
   buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (entry));  /* create a text buffer */
   gtk_text_buffer_get_end_iter (buffer, &end);                /* mark the end of the text buffer */
   end_mark = gtk_text_buffer_create_mark (buffer, NULL, &end, FALSE);
   gtk_text_buffer_insert (buffer, &end, reply_local, -1);     /* insert text in the buffer */
   gtk_text_buffer_get_start_iter (buffer, &start);            /* re-establish start and end of buffer */
   gtk_text_buffer_get_end_iter (buffer, &end);
      
   /* Pack widgets and display */
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),label);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),entry);
   gtk_widget_show_all (askbox);
                      
   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   /* Run this as a while loop 'no_valid-event' so we can call other widgets, for 
      example, a help dialog directly from here */
   no_valid_event = TRUE;
   while ( no_valid_event) {
     result = gtk_dialog_run (GTK_DIALOG (askbox));
     switch (result) {
       case GTK_RESPONSE_OK:
       /* 
       Terminate this string properly for return to fortran 
       */
         gtk_text_buffer_get_bounds (buffer, &start, &end);
         reply_local = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
/* debug         g_print ("local is %s",reply_local); */
         g_stpcpy (reply,reply_local);
/* debug         g_print ("reply is %s",reply); */
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       case 99:
         *ier=-2;
         no_valid_event = FALSE;
         break;
       default:
         *ier=-1;
         no_valid_event = FALSE;
         break;
       }
     }
   gtk_widget_destroy (askbox);
   
}
