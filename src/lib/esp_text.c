/* Functions to display text feedback, including warnings and errors. 

   Functions:
     inserttext   - add text to the text feedback area
     insert_dialog   - add text to the dialog area
     quick_message - display an error message
*/
   
#include <stdio.h>
#include <gtk/gtk.h>
#include "esp-r.h"

void inserttext_ ( char *message , int len)
{
   GtkTextBuffer *buffer;
   GtkTextIter end;
   GtkTextMark *end_mark;
   gchar *message2;
   
   /* 
      Create the widgets - first terminate the string 
   */
   message2 = g_strndup(message, (gsize) len);
 
   buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
   gtk_text_buffer_get_end_iter (buffer, &end);
   end_mark = gtk_text_buffer_create_mark (buffer, NULL, &end, FALSE);

   gtk_text_buffer_insert (buffer, &end, message2, -1);
   gtk_text_buffer_insert (buffer, &end, "\n", -1);
   
   /*
      Move the scrollbar down to expose the new text.
   */
   gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (text), end_mark, 0.0, TRUE, 0.0, 1.0);
   g_free (message2);
   return;
}
   
/* Function to display dialog (passed as msg1 & msg2 in dialog region. */
void msgbox_ (char *msg1, char *msg2, int len1, int len2) 
{
   GtkTextBuffer *buffer;
   GtkTextIter end;
   GtkTextMark *end_mark;
   gchar *msg1_local, *msg2_local;
   int msg1_l,msg2_l; /* non-blank lengths for prompt */
   
   /* 
      Create the widgets - first terminate the string 
   */
   msg1_l = msg2_l =0;
   f_to_c_l(msg1,&len1,&msg1_l);
   f_to_c_l(msg2,&len2,&msg2_l);
   msg1_local = g_strndup(msg1, (gsize) msg1_l);
   msg2_local = g_strndup(msg2, (gsize) msg2_l);
   buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (dialog));
   gtk_text_buffer_get_end_iter (buffer, &end);
   end_mark = gtk_text_buffer_create_mark (buffer, NULL, &end, FALSE);

   gtk_text_buffer_insert (buffer, &end, msg1_local, -1);
   gtk_text_buffer_insert (buffer, &end, "\n", -1);
   gtk_text_buffer_insert (buffer, &end, msg2_local, -1);
   gtk_text_buffer_insert (buffer, &end, "\n", -1);
   
   /*
      Move the scrollbar down to expose the new text.
   */
   gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (dialog), end_mark, 0.0, TRUE, 0.0, 1.0);
   g_free (msg1_local);
   g_free (msg2_local);
   return;
}
  
void continuebox_ (char *msg1, char *cnt, int len1, int lencnt) 
{

   GtkWidget *c_dialog;
   gchar *msg1_local, *cnt_local, *message;
   int msg1_l; /* non-blank lengths for prompt */
   
   /* 
      Create the widgets - first terminate the string 
   */
   msg1_l =0;
   f_to_c_l(msg1,&len1,&msg1_l);
   message = g_strndup(msg1, (gsize) msg1_l);
   cnt_local = g_strndup(cnt, (gsize) lencnt);	/* not yet used */

   c_dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_WARNING,
                                  GTK_BUTTONS_CLOSE,
                                  message);
   gtk_dialog_run (GTK_DIALOG (c_dialog));
   gtk_widget_destroy (c_dialog);
   return;
}

void clrhelp_ (void)
{
   /* clear help buffer, for next help message */
   help = "  ";
   help_width = 0;	/* reset width of help */
   help_lines = 0;	/* reset active help lines */
   
}

void updhelp_ ( char *message , int len)
{
   gchar *message_local;
   int lnq1;	/* for non-blank length */
   
   /* 
      Create the widgets - first terminate the string 
   */
   lnq1 = 0;
   f_to_c_l(message,&len,&lnq1);
   if(lnq1 >= help_width) help_width = lnq1;	/* update the maximum width */
   help_lines = help_lines + 1;	/* increment active help lines */
   message_local = g_strndup(message, (gsize) lnq1);
 
/* debug fprintf(stderr,"adding help text, %d %s\n",help_lines,message_local); */
   help = g_strjoin("\n",help,message_local,NULL);
   g_free (message_local);
   return;
   
}

/* *** for use with egphelpscroll *** */
static void set_policy (GtkWidget *sw)
{
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);  
}
   
/* *** egphelpscroll_() Display help text (more than 20 lines) in a new window. *** */
/* 
   Using same call variables as old version (although nothing is done 
   with these values, yet!
   << look for way to control placement of popup >>
   << this needs to have an extra button in response to value of *more >>
 */
void egphelpscroll_ (long int *IBX,long int *IBY,long int *IPFLG,long int *more,long int *iuresp)
{
   PangoFontDescription *pfd;	/* to hold test font */
   PangoContext *context;	/* for use in finding font properties */
   PangoFontMetrics *metrics;
   GtkWidget *h_dialog,  *sw, *view;
   gint menu_pix_hi;
   gint menu_pix_wd;
   gint f_height;	/* pixel height of default font */
   gint f_width;	/* pixel width of default font */
   
   /* 
      Use the h_dialog widget and the contents of the help buffer.
	  Do not attach to main window as help is displayed behind dialog boxes when called from them.
   */
   h_dialog = gtk_message_dialog_new (NULL,
      GTK_DIALOG_DESTROY_WITH_PARENT,GTK_MESSAGE_INFO,GTK_BUTTONS_CLOSE,"Help...");

/* react to different disp_fnt in order to set the size of the popup box */
   if (disp_fnt == 0 ) {
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 8");
#else
     pfd = pango_font_description_from_string("Serif,Medium 8");
#endif
/* debug     fprintf(stderr,"popup font medium 8\n"); */
   } else if (disp_fnt == 1 ) {	
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 10");
#else
     pfd = pango_font_description_from_string("Serif,Medium 10");
#endif
/* debug     fprintf(stderr,"popup font medium 10\n"); */
   } else if (disp_fnt == 2 ) {	
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 12");
#else
     pfd = pango_font_description_from_string("Serif,Medium 12");
#endif
/* debug     fprintf(stderr,"popup font medium 12\n"); */
   } else if (disp_fnt == 3 ) {
#ifdef SUN
     pfd = pango_font_description_from_string("Courier,Medium 14");
#else
     pfd = pango_font_description_from_string("Serif,Medium 14");
#endif
/* debug     fprintf(stderr,"configure font medium 14\n"); */
   } else if (disp_fnt == 4 ) {
     pfd = pango_font_description_from_string("Courier,Medium 8");
/* debug     fprintf(stderr,"configure courier medium 8\n"); */
   } else if (disp_fnt == 5 ) {
     pfd = pango_font_description_from_string("Courier,Medium 10");
/* debug     fprintf(stderr,"configure courier medium 10\n"); */
   } else if (disp_fnt == 6 ) {
     pfd = pango_font_description_from_string("Courier,Medium 12");
/* debug     fprintf(stderr,"configure courier medium 12\n"); */
   } else if (disp_fnt == 7 ) {
     pfd = pango_font_description_from_string("Courier,Medium 14");
/* debug     fprintf(stderr,"configure courier medium 14\n"); */
   }
   gtk_widget_modify_font(h_dialog, pfd);
   context = gtk_widget_get_pango_context (h_dialog);
   metrics = pango_context_get_metrics (context, pfd,
             pango_context_get_language (context));
   f_height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
           pango_font_metrics_get_descent (metrics));
   f_width = PANGO_PIXELS (pango_font_metrics_get_approximate_digit_width (metrics));
   pango_font_metrics_unref (metrics);

   /* fprintf(stderr,"font height and width is %d %d\n", f_height,f_width);  debug */
   menu_pix_wd = (gint) help_width * f_width +20;
   if(help_lines <= 7) {
     menu_pix_hi = 9 * (f_height + 2) +20;
   } else if (help_lines >= 8 && help_lines <= 18) {
     menu_pix_hi = (help_lines +2) * (f_height + 2) +20;
   } else {
     menu_pix_hi = 20 * (f_height + 2) +20;
   }
   /* fprintf(stderr,"help lines chars and pix wd %ld %ld %d\n", help_lines,help_width,menu_pix_wd);  debug */
   /* gtk_widget_set_size_request (h_dialog, menu_pix_wd, menu_pix_hi); */

/* implement scrolling (idea from gtkaboutdialog.c)... */
   gtk_dialog_set_default_response (GTK_DIALOG (h_dialog), GTK_BUTTONS_CLOSE);
   sw = gtk_scrolled_window_new (NULL, NULL);
   gtk_widget_set_size_request (sw, menu_pix_wd, menu_pix_hi);
   gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (sw),GTK_SHADOW_IN);
   gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				   GTK_POLICY_NEVER,GTK_POLICY_AUTOMATIC);
   g_signal_connect (sw, "map", G_CALLBACK (set_policy), NULL);
   gtk_box_pack_start (GTK_BOX (GTK_DIALOG (h_dialog)->vbox), sw, TRUE, TRUE, 0);

/* now set the font size to be used for the text in the scroll box. */
   view = gtk_text_view_new ();
   gtk_widget_modify_font(view, pfd);
   pango_font_description_free(pfd);

   gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)), help, -1);

   gtk_text_view_set_left_margin (GTK_TEXT_VIEW (view), 8);
   gtk_text_view_set_right_margin (GTK_TEXT_VIEW (view), 8);
   gtk_text_view_set_editable(GTK_TEXT_VIEW (view), FALSE);

   gtk_container_add (GTK_CONTAINER (sw), view);
   gtk_widget_show_all (h_dialog);

   /* Allow enter key to close dialog box */
   gtk_dialog_set_default_response (GTK_DIALOG (h_dialog), GTK_RESPONSE_CLOSE);

   gtk_dialog_run (GTK_DIALOG (h_dialog));
   gtk_widget_destroy (h_dialog);
   return;
}
  
   
