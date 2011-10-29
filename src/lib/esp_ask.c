/* Functions to ask for user input.

   Functions:
      askdialog - prompt for a text entry (with help, default and cancel options).
      askdialogcmd - prompt for a text entry (with help, default, cancel, alternative cmd options).
      askdialogcncl - prompt for a text entry (with help, default, user specified cancel cmd options).
      askdialog2cmd - prompt for a text entry (with help, default, cancel, two alternative cmd options).

*/

#include <stdio.h>
#include <gtk/gtk.h>
#include <glib.h>
#include "esp-r.h"

/* ***** askdialog_() ask for text input */
/* Could pass in:
    title for the pop up box
*/
void askdialog_(char *q1, char *reply,long int *ier, int lenq1, int lenrep)
/* q1 is the question being posed to the user
   reply is the reply but has the current value from the fortran side
   ier tracks if the default (-2) or cancel buttons (-3) have been selected (requires
     a suitable actions on the fortran side).
   lenq1 is passed from fortran as the length of the question
   lenrep is passed from fortran as the length of the answer text (which might be
     a blank or a word or phrase
*/
{
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;     /* local string for the users reply */
   gchar *question_local;  /* local string for the prompt */
   gint result;
   int no_valid_event;
   int lnq1,lnrep;	/* for non-blank lengths */
   gint lnblankstr;     /* for the blank string */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   char blankstr[96];
   
   *ier=0;	/* Reset value of ier flag */

/* create a string full of blanks for use in clearing reply_local */
   strcpy(blankstr,"                                                                                         ");
   lnblankstr=lenrep-1;

   f_to_c_l(reply,&lenrep,&lnrep);  /* find actual length of the string to be edited. */
   reply_local = g_strndup(reply, (gsize) lnblankstr);	/* start the text highlighted */

/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);

/* debug g_print("askdialog phrase %s\n",question_local);  */
/* debug g_print("askdialog_ text is %s\n",reply_local);  */
/* debug g_print("askdialog nb of help lines %d\n",help_lines);  */
    
   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Text  request",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_entry_new ();

   /* Define entry box properties */
   gtk_label_set_line_wrap(GTK_LABEL (label), TRUE);
/*    gtk_entry_set_max_length (GTK_ENTRY (entry), lenrep);	editing box allows up to lenrep max characters */
   gtk_entry_set_text (GTK_ENTRY (entry), reply_local);
   gtk_entry_set_activates_default(GTK_ENTRY (entry), TRUE);
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
       /* Terminate this string properly for return to fortran via strcpy. */
         reply_local = g_strndup(blankstr, (gsize) lnblankstr);  /* fill result with blanks first */
         /* g_print ("askdialog local %d is %s\n",lnblankstr,reply_local);  debug */
         reply_local = gtk_editable_get_chars(GTK_EDITABLE (entry),0,lnblankstr);
         strcpy(reply,reply_local);  /* copy from gstring to string return buffer */
         /* g_print ("askdialog reply is now %s",reply);   debug */
         g_free (reply_local);
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-3;
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
   ier tracks if the default (-2), cmd option (2) or cancel buttons (-3) have been selected (requires
     a suitable actions on the fortran side).
   lenq1 is passed length of prompt, lenrep is passed length of reply, lencmd is passed length of cmd.
*/
{
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;     /* local string for the users reply */
   gchar *question_local;  /* local string for the prompt */
   gchar *cmd_local;
   gint result;
   int no_valid_event;
   int lnq1,lnrep,lncmd1;	/* for non-blank lengths */
   gint lnblankstr;		/* for the blank string */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   char blankstr[96];

   *ier=0;	/* Reset value of ier flag */

/* create a string full of blanks for use in clearing reply_local */
   strcpy(blankstr,"                                                                                         ");
   lnblankstr=lenrep-1;

   f_to_c_l(reply,&lenrep,&lnrep);  /* find actual length of the string to be edited. */
   reply_local = g_strndup(reply, (gsize) lnblankstr);	/* start with the text highlighted */

/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);
   lncmd1 = 0;
   f_to_c_l(cmd,&lencmd,&lncmd1);
   cmd_local = g_strndup(cmd, (gsize) lncmd1);

/* debug g_print("phrase %s\n",question_local); */
/* debug g_print("text is %s\n",reply_local); */
/* debug g_print("cmd is %s\n",cmd_local); */
/* debug g_print("nb of help lines %d\n",help_lines); */

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
/*   gtk_entry_set_max_length (GTK_ENTRY (entry), lenrep);	editing box allows up to lenrep max characters */
   gtk_entry_set_text (GTK_ENTRY (entry), reply_local);
   gtk_entry_set_activates_default(GTK_ENTRY (entry), TRUE);
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
       /* Terminate this string properly for return to fortran via strcpy. */
         reply_local = gtk_editable_get_chars(GTK_EDITABLE (entry),0,lnblankstr);
         strcpy(reply,reply_local);  /* copy from gstring to string return buffer */
         /* g_print ("askdialogcmd reply is now %s",reply);   debug */
         g_free (reply_local);
	 no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-3;
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

/* ***** askdialogcncl_() ask for text input with user defined cancel option */
/* This matches openaskcnclbox and askcncldialog in the X11 code where a specific
 * string is passed for the go-back/cancel opion. In this case the GTK supplied
 * cancel button is not needed.
*/
void askdialogcncl_(char *q1, char *reply, char *cncl, long int *ier, int lenq1, int lenrep, int lencncl)
/* q1 is the question being posed to the user
   reply is the reply but has the current value from the fortran side
   cncl is an cancel command string from the fortran side
   ier tracks if the default (-2), cncl option (2) have been selected (requires
     a suitable actions on the fortran side).
   lenq1 is passed length of prompt, lenrep is passed length of reply, lencncl is passed length of cncl.
*/
{
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;     /* local string for the users reply */
   gchar *question_local;  /* local string for the prompt */
   gchar *cncl_local;
   gint result;
   int no_valid_event;
   int lnq1,lnrep,lncncl1;	/* for non-blank lengths */
   gint lnblankstr;		/* for the blank string */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   char blankstr[96];

   *ier=0;	/* Reset value of ier flag */

/* create a string full of blanks for use in clearing reply_local */
   strcpy(blankstr,"                                                                                         ");
   lnblankstr=lenrep-1;

   f_to_c_l(reply,&lenrep,&lnrep);  /* find actual length of the string to be edited. */
   reply_local = g_strndup(reply, (gsize) lnblankstr);	/* start with the text highlighted */

/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);
   lncncl1 = 0;
   f_to_c_l(cncl,&lencncl,&lncncl1);
   cncl_local = g_strndup(cncl, (gsize) lncncl1);

/* debug g_print("phrase %s\n",question_local); */
/* debug g_print("text is %s\n",reply_local); */
/* debug g_print("cncl is %s\n",cncl_local); */
/* debug g_print("nb of help lines %d\n",help_lines); */

   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Text request with go-back option",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,cncl_local,98,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_entry_new ();

   /* Define entry box properties */
   gtk_label_set_line_wrap(GTK_LABEL (label), TRUE);
/*   gtk_entry_set_max_length (GTK_ENTRY (entry), lenrep);	editing box allows up to lenrep max characters */
   gtk_entry_set_text (GTK_ENTRY (entry), reply_local);
   gtk_entry_set_activates_default(GTK_ENTRY (entry), TRUE);
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
       /* Terminate this string properly for return to fortran via strcpy. */
         reply_local = g_strndup(blankstr, (gsize) lnblankstr);  /* fill result with blanks first */
         reply_local = gtk_editable_get_chars(GTK_EDITABLE (entry),0,lnblankstr);
         strcpy(reply,reply_local);  /* copy from gstring to string return buffer */
         /* g_print ("askdialogcncl reply is now %s",reply);   debug */
         g_free (reply_local);
	 no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
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
   ier tracks if the default (-2), cmd1 option (2) cmd2 option (3) or cancel buttons (-3) have been
     selected (requires a suitable actions on the fortran side).
   lenq1 is passed length of prompt, lenrep is passed length of reply, lencmd is passed length of cmd.
*/
{
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;     /* local string for the users reply */
   gchar *question_local;  /* local string for the prompt */
   gchar *cmd1_local;
   gchar *cmd2_local;
   gint result;
   int no_valid_event;
   int lnq1,lnrep,lncmd1,lncmd2;	/* for non-blank lengths */
   gint lnblankstr;     /* for the blank string */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */
   char blankstr[96];

   *ier=0;	/* Reset value of ier flag */

/* create a string full of blanks for use in clearing reply_local */
   strcpy(blankstr,"                                                                                         ");
   lnblankstr=lenrep-1;

   f_to_c_l(reply,&lenrep,&lnrep);  /* find actual length of the string to be edited. */
   reply_local = g_strndup(reply, (gsize) lnblankstr);	/* start with text highlighted */

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

/* debug g_print("phrase %s\n",question_local); */
/* debug g_print("text is %s\n",reply_local); */
/* debug g_print("cmd1 is %s\n",cmd1_local); */
/* debug g_print("cmd2 is %s\n",cmd2_local); */
/* debug g_print("nb of help lines %d\n",help_lines); */

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
/*   gtk_entry_set_max_length (GTK_ENTRY (entry), lenrep);	editing box allows up to lenrep max characters */
   gtk_entry_set_text (GTK_ENTRY (entry), reply_local);
   gtk_entry_set_activates_default(GTK_ENTRY (entry), TRUE);
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
       /* Terminate this string properly for return to fortran via strcpy. */
         reply_local = g_strndup(blankstr, (gsize) lnblankstr);  /* fill result with blanks first */
         reply_local = gtk_editable_get_chars(GTK_EDITABLE (entry),0,lnblankstr);
         strcpy(reply,reply_local);  /* copy from gstring to string return buffer */
         /* g_print ("askdialog2cmd reply is now %s",reply);   debug */
         g_free (reply_local);
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-3;
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


/* ***** askdialog248_() ask for multi-line text input */
/* Could pass in:
    title for the pop up box
*/
void askdialog248_(char *q1, char *reply,long int *ier, int lenq1, int lenrep)
/* q1 is the question being posed to the user
   reply is the reply but has the current value from the fortran side
   ier tracks if the default (-2) or cancel (-3) buttons have been selected (requires
     a suitable actions on the fortran side).
   lenq1 is passed from fortran as the length of the question
   lenrep is passed from fortran as the length of the answer text (which might be
     a blank or a word or phrase
*/
{
   GtkTextBuffer *buffer;
   GtkTextIter start;
   GtkTextIter end;
   GtkTextMark *end_mark;
   GtkWidget *askbox, *entry, *label;
   gchar *reply_local;     /* local string for the users reply */
   gchar *question_local;  /* local string for the prompt */
   gint result;
   gint menu_pix_hi;
   gint menu_pix_wd;
   int no_valid_event;
   int lnq1,lnrep;	/* for non-blank length */
   long int ibx,iby,more;	/* set default position of help */
   long int ipflg,iuresp;	/* response from pop-up help */

   *ier=0;	/* Reset value of ier flag */
   
/* This function does not use the blankstr pattern for clearing the string
   to be edited. It starts reply_local with a the actual text size which
   seems to help the word warp to work properly.
*/
   f_to_c_l(reply,&lenrep,&lnrep);  /* find actual length of the string to be edited. */

   if (lnrep <= 2 ) {
     strcpy ( reply_local, "  "); /* if starting with blank do this to help warp */
   } else {
     reply_local = g_strndup(reply, (gsize) lnrep); /* only fill reply_local with actual text */
   }

/* find out actual length of each prompt and then total length with a space between. */
   lnq1 = 0;
   f_to_c_l(q1,&lenq1,&lnq1);
   question_local = g_strndup(q1, (gsize) lnq1);

/* debug g_print("phrase %s\n",question_local); */
/* debug g_print("text is %s\n",reply_local); */
/* debug g_print("nb of help lines %d\n",help_lines) */;

   /* Create the widgets */
   askbox = gtk_dialog_new_with_buttons("Text request",
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,"Use default", 99,
     GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);

   label = gtk_label_new (question_local);
   entry = gtk_text_view_new ();  /* create a multi-line text editing area */
   gtk_text_view_set_wrap_mode ( GTK_TEXT_VIEW (entry), GTK_WRAP_WORD );       /* warp the words */
   gtk_text_view_set_cursor_visible ( GTK_TEXT_VIEW (entry), TRUE );       /* show insertion point */
   buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (entry));  /* create a text buffer */
   gtk_text_buffer_get_end_iter (buffer, &end);                /* mark the end of the text buffer */
   end_mark = gtk_text_buffer_create_mark (buffer, NULL, &end, FALSE);
   gtk_text_buffer_insert (buffer, &end, reply_local, -1);     /* insert text in the buffer */
   gtk_text_buffer_get_start_iter (buffer, &start);            /* re-establish start and end of buffer */
   gtk_text_buffer_get_end_iter (buffer, &end);
   gtk_entry_set_activates_default(GTK_ENTRY (entry), TRUE);

   menu_pix_wd = 420;    /* try 420 pixel wide popup */
   menu_pix_hi = 200;    /* try 200 pixel wide popup */
   gtk_widget_set_size_request (askbox, menu_pix_wd, menu_pix_hi);

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
         lnq1 = 0;	/* detect the actual size of reply_local */
         f_to_c_l(reply_local,&lenrep,&lnq1);
/* debug g_print ("askdialog248 local %d is %s",lnq1,reply_local); */
         g_snprintf (reply, (gulong)lnq1+1, "%s", reply_local); /* copy reply_local into reply +1 for null */
/* debug g_print ("askdialog248 reply is %s",reply); */
         g_free (reply_local);
         no_valid_event = FALSE;
         break;
       case GTK_RESPONSE_HELP:
         ibx= 0; iby= 0; more = 0;
         egphelpscroll_(&ibx,&iby,&ipflg,&more,&iuresp);
         break;
       case GTK_RESPONSE_CANCEL:
         *ier=-3;
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
