/*
 Functions to create, display and manage an ESP-r type menu and abcd...g box
   espmenuinit_ () initialise menu - using list for entries
   updmenu_() initialise popup menu text array
   espmenuitems_ () adds an item to the menu.
   espmenu_ () display menu and wait for selection to be made
   abcbox_callback() actions when an abc... choice is made.
   espabcbox_ () initialise abcbox - will use radio buttons for entries
   espdozenbox_ () initialise dozen choices use radio buttons for entries
   okbox_callback() actions when an ok... choice is made.

 */
static const char rcsid[] = "$Id$";

#include <stdio.h>
#include <gtk/gtk.h>
#include <esp_list.h>
#include <gtk/gtkdialog.h>
#include <string.h>
#include <esp-r.h>

/* extern void g_get_esp_item_from_list_cb(); from popup */

/* required by e_menu list and callbacks */
static const gchar *list_item_data_key = "esp_list_item_data";

GtkWidget *gtkplist;	/* the list used for popup menus */
GtkWidget *menu_items;
gint menu_pick;
gint pmenu_pick;

gint abc_pick;

/* **** g_get_esp_item_from_list_cb is the call-back for "selection_changed"
 * signal of the menu selection process */
void g_get_esp_item_from_list_cb ( GtkWidget *a_list,
				   gpointer  func_data )
{
  GList *dlist;
  gchar *buf;

  /* Fetch the selected items of the List, treat as read-only! */
  dlist = GTK_LIST (a_list)->selection; /* listbox widget */

  if (!dlist) {
    esp_selected = buf = NULL; /* If deselected, nullify the strings */
    return;
  }

/* Get the item from the doubly linked list,
 * query the data associated with list_item_data_key.
 * then write it to global (esp_selected) */
  buf = g_object_get_data (G_OBJECT (dlist->data), list_item_data_key);

  got_item_n = gtk_list_child_position ((GtkList*)a_list, dlist->data);
  esp_selected = (gchar*) strdup(buf);
/* debug g_print("you clicked item %d: %s -\n", got_item_n, buf); */
  g_main_loop_quit (menu_loop);
}


/* *** espmenuinit_ () initialise menu - using gtk_list_new for entries *** */
void espmenuinit_ (char *title, int len) {
   gchar *title2,*str,*title3;
   GtkWidget *label, *a_list_item;
   GtkWidget *title_frame;
   gint f_height;	/* pixel height of default font */

   /* DEBUG...
      fprintf(stderr,"TITLE %s\n",title);
      fprintf(stderr,"LEN %d\n",len);
   */

/* create the outer frame 5 pixels wide */
   menu_frame = gtk_vbox_new (FALSE,5);
   gtk_container_set_border_width (GTK_CONTAINER (menu_frame), 1);
   gtk_container_add (GTK_CONTAINER (emenu), menu_frame);
   gtk_widget_show ( menu_frame);

/* create a scolling window for the menu.
 * << still to be done is to set it so that scroll does not
 * << show when there is no need to scroll
 */
   e_scrolled_window = gtk_scrolled_window_new (NULL, NULL);
   gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (e_scrolled_window),
     GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC);
   gtk_container_add (GTK_CONTAINER (menu_frame), e_scrolled_window);
   gtk_widget_set_size_request (e_scrolled_window, -1, 500);
   gtk_widget_show (e_scrolled_window);

/* setup the list structure within e_scrolled_window */
   gtklist = gtk_list_new ();
   gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (e_scrolled_window),gtklist);
   gtk_widget_show (gtklist);

   g_signal_connect (G_OBJECT (gtklist), "selection_changed",
                     G_CALLBACK (g_get_esp_item_from_list_cb),NULL);

/* Include the menu title as the first item in the menu. Use g_strndup to
 * go from fortran and then use g_strjoin to prepend a space. */
   title2 = g_strndup(title, (gsize) len);
/*   title3 = g_strjoin(" ","<bold>",title2,"</bold>",NULL);  for when menomonics avail */
   title3 = g_strjoin(" ",title2,NULL);
   label = gtk_label_new (title3);
/*   gtk_label_set_markup_with_mnemonic (GTK_LABEL (label), title3); */
   a_list_item = gtk_list_item_new_with_label (title3);

   gtk_container_add (GTK_CONTAINER (gtklist), a_list_item);
   gtk_widget_show (a_list_item);
   gtk_label_get (GTK_LABEL (label), &str);
   g_object_set_data (G_OBJECT (a_list_item), list_item_data_key, str);
   g_free (title2);

   gtk_widget_show_all (emenu); /* Display the new widgets. */
}


/* *************** ESRU menu text update. *************** */
/*
 This function takes an array of strings from f77 and stores
 it in the static array m_list for subsequent use by other functions.
*/
/* char m_list[MENU_LIST_LEN][125]; character arrays for menu buffer */
/* char mtype_list[MENU_LIST_LEN]; character array representing m_list array use */
/* int m_width = 0; current menu max line length */
/* int m_lines = 0; current number of active menu lines */
void updmenu_(items,itypes,nitmsptr,iw,len_items)
  char      *items;         /* f77 array of menu text strings    */
  char      *itypes;        /* f77 character array (nitmsptr wide)    */
  long int  *nitmsptr;      /* number of menu lines to display   */
  long int  *iw;            /* actual max char width in items    */
  int  len_items;           /* length of menu string from f77    */
{
  int	i, j, k;
  int	pm_line = (int) *nitmsptr;

  pm_width = (int) *iw;	/* remember width of menu text */
  pm_lines = pm_line;	/* remember number of lines */
  if(pm_lines == 0)return;	/* don't bother if no lines */
  strncpy(pmtype_list,itypes,(unsigned int)pm_lines);	/* copy to static array */

  k = 0;
  for(i = 0; i < pm_lines; i++) {	/* for each line...  */
    for(j = 0; j < len_items; j++) {	/* for each character...  */
      pm_list[i][j] = items[k];
      k = k +1;   /* increment for next char in items (a fortran string array does not have
                     nulls between strings in array, it just looks like one long string) */
    }
    pm_list[i][len_items] = '\0';	/* write terminator  */
/* debug  fprintf(stderr,"pm_list %s %d %d %d %d\n",pm_list[i],i,k,pm_width,len_items);  */
  }
  return;
}

/* *** espmenuitems_ () adds an item to the e_menu *** */
void espmenuitems_ (char *item,long int *ino, int len)
{

   GtkWidget *label, *a_list_item;
   gchar *item2,*str;

   item2 = g_strndup(item, (gsize) len);

   /* DEBUG...
      fprintf(stderr,"espmenuitems INO LEN %ld %d %s\n",*ino,len,item2);
    */
   label = gtk_label_new (item2);
   a_list_item = gtk_list_item_new_with_label (item2);

   gtk_container_add (GTK_CONTAINER (gtklist), a_list_item);
   gtk_widget_show (a_list_item);
   gtk_label_get (GTK_LABEL (label), &str);
   g_object_set_data (G_OBJECT (a_list_item), list_item_data_key, str);
   g_free (item2);
   return;
}

/* *** espmenu_ () display menu and wait for selection to be made *** */
void espmenu_ (int *ino)
{

/* Show menu and reset selection. */
   gtk_widget_show_all (gtklist);
   menu_pick = 0;

   /* DEBUG...
    * fprintf(stderr,"INO1 %d\n",*ino);
    * fprintf(stderr,"INO2 %d\n",menu_pick);
    */

/* Create a new event loop and run it.  When a selection is
 * made the value of got_item_n will equal to the position in
 * the list (because the menu title is the first item).
 * This value is placed to the location that ino points to.
 */
   g_main_loop_run (menu_loop);
   *ino = got_item_n;	/* got_item_n is global set by g_get_esp_item_from_list_cb */

   /* DEBUG...
    * fprintf(stderr,"INO4 %d\n",*ino);
    * fprintf(stderr,"INO5 %d\n",got_item_n);
    */
   /* get rid of the entities used in the previous dynamic menu */
    gtk_widget_destroy (gtklist);
    gtk_widget_destroy (e_scrolled_window);
    gtk_widget_destroy (menu_frame);
}


/* *** abcbox_callback() actions when an abc... choice is made. *** */

void abcbox_callback( GtkWidget *widget,
                      gpointer  data )
{
/* debug g_print ("Hello again - %d was pressed\n", GPOINTER_TO_INT (data)); */
  abc_pick = GPOINTER_TO_INT (data);	/* see spinbutton example */
}

/* *** espabcbox_ () initialise abcbox - will use radio buttons for entries *** */
void espabcbox_ (char *msg1, char *aopt, char *bopt, char *copt,
                 char *dopt, char *eopt, char *fopt, char *gopt, long int *ipick,
                 int msg1_len, int aopt_len, int bopt_len, int copt_len,
                 int dopt_len, int eopt_len, int fopt_len, int gopt_len) {

   GtkWidget *askbox, *hbox, *left_col, *right_col, *button, *label;
   GSList *group;
   gchar *title_local, *msg1_local;
   gchar *aopt_local, *bopt_local, *copt_local, *dopt_local, *eopt_local, *fopt_local;
   gchar *gopt_local;
   gchar *question_local;
   gint result;
   gint aopt_l,bopt_l,copt_l,dopt_l,eopt_l,fopt_l,gopt_l; /* non-blank lengths for options */
   int msg1_l; /* non-blank lengths for prompt */

   title_local = "  ";

/* find out actual length of each prompt and then total length with a space between. */
   msg1_l = 0;
   f_to_c_l(msg1,&msg1_len,&msg1_l);
   question_local = g_strndup(msg1, (gsize) msg1_l);
/* debug  fprintf(stderr,"ask phrase %s\n",question_local); */

   aopt_l = bopt_l = copt_l = dopt_l = eopt_l = fopt_l = gopt_l = 0;
   f_to_c_l(aopt,&aopt_len,&aopt_l);
   aopt_local = g_strndup(aopt, (gsize) aopt_l);
   f_to_c_l(bopt,&bopt_len,&bopt_l);
   bopt_local = g_strndup(bopt, (gsize) bopt_l);
   f_to_c_l(copt,&copt_len,&copt_l);
   copt_local = g_strndup(copt, (gsize) copt_l);
   f_to_c_l(dopt,&dopt_len,&dopt_l);
   dopt_local = g_strndup(dopt, (gsize) dopt_l);
   f_to_c_l(eopt,&eopt_len,&eopt_l);
   eopt_local = g_strndup(eopt, (gsize) eopt_l);
   f_to_c_l(fopt,&fopt_len,&fopt_l);
   fopt_local = g_strndup(fopt, (gsize) fopt_l);
   f_to_c_l(gopt,&gopt_len,&gopt_l);
   gopt_local = g_strndup(gopt, (gsize) gopt_l);
/* debug fprintf(stderr,"non-blank lengths are %d %d %d %d %d %d %d\n",aopt_l,bopt_l,copt_l,dopt_l,eopt_l,fopt_l,gopt_l); */

   /* Set ok response, but if *ipick is zero reset to one. */
   abc_pick = (gint) *ipick;
   if ( abc_pick == 0 ) abc_pick = 1;

   /* Create the widgets: first the dialog window, then split the default
   vbox into two (left_col and right_col) by using a hbox so as the items
   can be displayed in two columns.
   */
   askbox = gtk_dialog_new_with_buttons(title_local,
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);
   label = gtk_label_new (question_local);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox), label);

   hbox = gtk_hbox_new (TRUE, 2);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),hbox);
   left_col = gtk_vbox_new (TRUE, 2);
   gtk_box_pack_start (GTK_BOX (hbox),left_col, TRUE, TRUE, 0);
   right_col = gtk_vbox_new (TRUE, 2);
   gtk_box_pack_start (GTK_BOX (hbox),right_col, TRUE, TRUE, 0);

   /* Add entries to the columns in turn */
    button = gtk_radio_button_new_with_label (NULL, aopt_local);
    gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
    if ( abc_pick==1 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
    g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (abcbox_callback), GINT_TO_POINTER (1));
    gtk_widget_show (button);
    group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button));

    button = gtk_radio_button_new_with_label (group, bopt_local);
    gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
    if ( abc_pick==2 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
    g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (abcbox_callback), GINT_TO_POINTER (2));
    gtk_widget_show (button);

    /* Additional entries.  Check if defined before displaying (string longer than 1 char) */
    if (copt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), copt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==3 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (3));
      gtk_widget_show (button);
    }

    if (dopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), dopt_local);
      gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
      if ( abc_pick==4 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (4));
      gtk_widget_show (button);
    }

    if (eopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), eopt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==5 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (abcbox_callback), GINT_TO_POINTER (5));
      gtk_widget_show (button);
    }

    if (fopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), fopt_local);
      gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
      if ( abc_pick==6 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (6));
      gtk_widget_show (button);
    }

    if (gopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), gopt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==7 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (7));
      gtk_widget_show (button);
    }

   /*
      Display the new widgets.
   */
   gtk_widget_show_all (askbox);

   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   result = gtk_dialog_run (GTK_DIALOG (askbox));
   switch (result)
      {
       case GTK_RESPONSE_OK:
       /*   fprintf(stderr,"Goodbye - %d %d was selected\n", abc_pick,result); debug */
          *ipick = (long int) abc_pick;
          break;
       case GTK_RESPONSE_CANCEL:
       /*   fprintf(stderr,"Goodbye - %d %d was original choice\n", abc_pick,result); debug */
          *ipick = -3;
          break;
       case GTK_RESPONSE_HELP:
       /*   fprintf(stderr,"Goodbye - %d %d help was requested\n", abc_pick,result); debug */
          *ipick = -8;
          break;
       default:
       /*   fprintf(stderr,"Goodbye - %d %d fell to default response\n", abc_pick,result); debug */
          *ipick = -1;
          break;
      }
   gtk_widget_destroy (askbox);

}

/* *** espdozenbox_ () initialise dozen choices use radio buttons for entries *** */
void espdozenbox_ (char *msg1, char *aopt, char *bopt, char *copt,
                 char *dopt, char *eopt, char *fopt, char *gopt,
                 char *hopt, char *iopt, char *jopt, char *kopt,
                 char *lopt, long int *ipick,
                 int msg1_len, int aopt_len, int bopt_len, int copt_len,
                 int dopt_len, int eopt_len, int fopt_len, int gopt_len,
                 int hopt_len, int iopt_len, int jopt_len, int kopt_len,
                 int lopt_len) {

   GtkWidget *askbox, *hbox, *left_col, *right_col, *button, *label;
   GSList *group;
   gchar *title_local, *msg1_local;
   gchar *aopt_local, *bopt_local, *copt_local, *dopt_local, *eopt_local, *fopt_local;
   gchar *gopt_local, *hopt_local, *iopt_local, *jopt_local, *kopt_local, *lopt_local;
   gchar *question_local;
   gint result;
   gint aopt_l,bopt_l,copt_l,dopt_l,eopt_l,fopt_l,gopt_l,hopt_l,iopt_l,jopt_l,kopt_l,lopt_l; /* non-blank lengths for options */
   int msg1_l; /* non-blank lengths for prompt */

   title_local = "  ";

/* find out actual length of each prompt and then total length with a space between. */
   msg1_l = 0;
   f_to_c_l(msg1,&msg1_len,&msg1_l);
   question_local = g_strndup(msg1, (gsize) msg1_l);
/* debug  fprintf(stderr,"ask phrase %s\n",question_local); */

   aopt_l = bopt_l = copt_l = dopt_l = eopt_l = fopt_l = gopt_l = 0;
   hopt_l = iopt_l = jopt_l = kopt_l = lopt_l = 0;
   f_to_c_l(aopt,&aopt_len,&aopt_l);
   aopt_local = g_strndup(aopt, (gsize) aopt_l);
   f_to_c_l(bopt,&bopt_len,&bopt_l);
   bopt_local = g_strndup(bopt, (gsize) bopt_l);
   f_to_c_l(copt,&copt_len,&copt_l);
   copt_local = g_strndup(copt, (gsize) copt_l);
   f_to_c_l(dopt,&dopt_len,&dopt_l);
   dopt_local = g_strndup(dopt, (gsize) dopt_l);
   f_to_c_l(eopt,&eopt_len,&eopt_l);
   eopt_local = g_strndup(eopt, (gsize) eopt_l);
   f_to_c_l(fopt,&fopt_len,&fopt_l);
   fopt_local = g_strndup(fopt, (gsize) fopt_l);
   f_to_c_l(gopt,&gopt_len,&gopt_l);
   gopt_local = g_strndup(gopt, (gsize) gopt_l);
   f_to_c_l(hopt,&hopt_len,&hopt_l);
   hopt_local = g_strndup(hopt, (gsize) hopt_l);
   f_to_c_l(iopt,&iopt_len,&iopt_l);
   iopt_local = g_strndup(iopt, (gsize) iopt_l);
   f_to_c_l(jopt,&jopt_len,&jopt_l);
   jopt_local = g_strndup(jopt, (gsize) jopt_l);
   f_to_c_l(kopt,&kopt_len,&kopt_l);
   kopt_local = g_strndup(kopt, (gsize) kopt_l);
   f_to_c_l(lopt,&lopt_len,&lopt_l);
   lopt_local = g_strndup(lopt, (gsize) lopt_l);
/* debug  fprintf(stderr,"non-blank lengths are %d %d %d %d %d %d %d %d %d %d %d %d\n",aopt_l,bopt_l,copt_l,dopt_l,eopt_l,
     fopt_l,gopt_l,hopt_l,iopt_l,jopt_l,kopt_l,lopt_l); */

   /* Set ok response, but if *ipick is zero reset to one. */
   abc_pick = (gint) *ipick;
   if ( abc_pick == 0 ) abc_pick = 1;

   /* Create the widgets: first the dialog window, then split the default
   vbox into two (left_col and right_col) by using a hbox so as the items
   can be displayed in two columns.
   */
   askbox = gtk_dialog_new_with_buttons(title_local,
     GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_STOCK_HELP, GTK_RESPONSE_HELP,GTK_STOCK_OK, GTK_RESPONSE_OK,
     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);
   label = gtk_label_new (question_local);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox), label);

   hbox = gtk_hbox_new (TRUE, 2);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),hbox);
   left_col = gtk_vbox_new (TRUE, 2);
   gtk_box_pack_start (GTK_BOX (hbox),left_col, TRUE, TRUE, 0);
   right_col = gtk_vbox_new (TRUE, 2);
   gtk_box_pack_start (GTK_BOX (hbox),right_col, TRUE, TRUE, 0);

   /* Add entries to the columns in turn */
    button = gtk_radio_button_new_with_label (NULL, aopt_local);
    gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
    if ( abc_pick==1 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
    g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (abcbox_callback), GINT_TO_POINTER (1));
    gtk_widget_show (button);
    group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button));

    button = gtk_radio_button_new_with_label (group, bopt_local);
    gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
    if ( abc_pick==2 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
    g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (abcbox_callback), GINT_TO_POINTER (2));
    gtk_widget_show (button);

    /* Additional entries.  Check if defined before displaying (string longer than 1 char) */
    if (copt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), copt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==3 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (3));
      gtk_widget_show (button);
    }

    if (dopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), dopt_local);
      gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
      if ( abc_pick==4 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (4));
      gtk_widget_show (button);
    }

    if (eopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), eopt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==5 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (abcbox_callback), GINT_TO_POINTER (5));
      gtk_widget_show (button);
    }

    if (fopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), fopt_local);
      gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
      if ( abc_pick==6 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (6));
      gtk_widget_show (button);
    }

    if (gopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), gopt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==7 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (7));
      gtk_widget_show (button);
    }

    if (hopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), hopt_local);
      gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
      if ( abc_pick==8 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (8));
      gtk_widget_show (button);
    }

    if (iopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), iopt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==9 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (9));
      gtk_widget_show (button);
    }

    if (jopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), jopt_local);
      gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
      if ( abc_pick==10 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (10));
      gtk_widget_show (button);
    }

    if (kopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), kopt_local);
      gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
      if ( abc_pick==11 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (11));
      gtk_widget_show (button);
    }

    if (lopt_l > 1 ) {
      button = gtk_radio_button_new_with_label_from_widget
                            (GTK_RADIO_BUTTON (button), lopt_local);
      gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
      if ( abc_pick==12 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
      g_signal_connect (G_OBJECT (button), "pressed",
                        G_CALLBACK (abcbox_callback), GINT_TO_POINTER (12));
      gtk_widget_show (button);
    }

   /*
      Display the new widgets.
   */
   gtk_widget_show_all (askbox);

   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   result = gtk_dialog_run (GTK_DIALOG (askbox));
   switch (result)
      {
       case GTK_RESPONSE_OK:
       /*   fprintf(stderr,"Goodbye - %d %d was selected\n", abc_pick,result); */
          *ipick = (long int) abc_pick;
          break;
       case GTK_RESPONSE_CANCEL:
       /*   fprintf(stderr,"Goodbye - %d %d was original choice\n", abc_pick,result); */
          break;
       case GTK_RESPONSE_HELP:
       /*   fprintf(stderr,"Goodbye - %d %d help was requested\n", abc_pick,result); */
          *ipick = -8;
          break;
       default:
       /*   fprintf(stderr,"Goodbye - %d %d fell to default response\n", abc_pick,result); */
          *ipick = -1;
          break;
      }
   gtk_widget_destroy (askbox);

}

/* *** okbox_callback() actions when an ok... choice is made. *** */

void okbox_callback( GtkWidget *widget,
                      gpointer  data )
{
/* debug g_print ("Hello again - %d was pressed\n", GPOINTER_TO_INT (data)); */
  abc_pick = GPOINTER_TO_INT (data);	/* see spinbutton example */
}

/* *** espokbox_ () initialise askok box - will use radio buttons for entries *** */
void espokbox_ (char *msg1, char *aopt, char *bopt, char *copt,
                long int *idef,long int *ipick, int msg1_len,
                int aopt_len, int bopt_len, int copt_len) {

   GtkWidget *askbox, *hbox, *left_col, *right_col, *button, *label;
   GSList *group;
   gchar *title_local;
   gchar *aopt_local, *bopt_local, *copt_local;
   gchar *question_local;
   gint result;
   gint aopt_l,bopt_l,copt_l; /* non-blank lengths */
   gint msg1_l; /* non-blank lengths for prompt */

/*
 * Create a frame for the menu to be displayed in, add
 * title and then add it to the menu area of the main window.
 */
   title_local = "  ";

/* find out actual length of prompt  */
   msg1_l = 0;
   f_to_c_l(msg1,&msg1_len,&msg1_l);

/* debug  fprintf(stderr,"prompt lengths are %d %d\n",msg1_len,msg1_l); */
   question_local = g_strndup(msg1, (gsize) msg1_l);
/* debug  fprintf(stderr,"ask phrase %s\n",question_local); */

   aopt_l = bopt_l = copt_l = 0;
   f_to_c_l(aopt,&aopt_len,&aopt_l);
   aopt_local = g_strndup(aopt, (gsize) aopt_l);
   f_to_c_l(bopt,&bopt_len,&bopt_l);
   bopt_local = g_strndup(bopt, (gsize) bopt_l);
   f_to_c_l(copt,&copt_len,&copt_l);
   copt_local = g_strndup(copt, (gsize) copt_l);
/* debug  fprintf(stderr,"non-blank lengths are %d %d %d\n",aopt_l,bopt_l,copt_l); */
   
   abc_pick = (gint) *idef;	/* Set default abc_pick response to current default */

   /* Create the widgets: first the dialog window, then split the default
   vbox into two (left_col and right_col) by using a hbox so as the items
   can be displayed in two columns.
   */
   if(*idef==1) {
     askbox = gtk_dialog_new_with_buttons(title_local,
       GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
       GTK_STOCK_HELP, GTK_RESPONSE_HELP,copt_local, 1,
       GTK_STOCK_OK, GTK_RESPONSE_OK,
       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);
   } else if(*idef==2) {
     askbox = gtk_dialog_new_with_buttons(title_local,
       GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
       GTK_STOCK_HELP, GTK_RESPONSE_HELP,copt_local, 2,
       GTK_STOCK_OK, GTK_RESPONSE_OK,
       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);
   } else if (idef==0) {
     askbox = gtk_dialog_new_with_buttons(title_local,
       GTK_WINDOW (window),GTK_DIALOG_DESTROY_WITH_PARENT,
       GTK_STOCK_HELP, GTK_RESPONSE_HELP,
       GTK_STOCK_OK, GTK_RESPONSE_OK,
       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,NULL);
   }

   label = gtk_label_new (question_local);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox), label);

   hbox = gtk_hbox_new (TRUE, 2);
   gtk_container_add (GTK_CONTAINER (GTK_DIALOG(askbox)->vbox),hbox);
   left_col = gtk_vbox_new (TRUE, 2);
   gtk_box_pack_start (GTK_BOX (hbox),left_col, TRUE, TRUE, 0);
   right_col = gtk_vbox_new (TRUE, 2);
   gtk_box_pack_start (GTK_BOX (hbox),right_col, TRUE, TRUE, 0);

   /* Add entries to the columns in turn */
    button = gtk_radio_button_new_with_label (NULL, aopt_local);
    gtk_box_pack_start (GTK_BOX (left_col), button, TRUE, TRUE, 0);
    if ( abc_pick==1 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
    g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (okbox_callback), GINT_TO_POINTER (1));
    gtk_widget_show (button);
    group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button));

    button = gtk_radio_button_new_with_label (group, bopt_local);
    gtk_box_pack_start (GTK_BOX (right_col), button, TRUE, TRUE, 0);
    if ( abc_pick==2 ) {gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);}
    g_signal_connect (G_OBJECT (button), "pressed",
                      G_CALLBACK (okbox_callback), GINT_TO_POINTER (2));
    gtk_widget_show (button);

   /*
      Display the new widgets.
   */
   gtk_widget_show_all (askbox);

   /* Set dialog properties and wait for user response */
   gtk_window_set_modal (GTK_WINDOW (askbox), TRUE);
   gtk_window_set_transient_for(GTK_WINDOW (askbox), GTK_WINDOW (window));

   result = gtk_dialog_run (GTK_DIALOG (askbox));
   switch (result)
      {
       case GTK_RESPONSE_OK:
       /*   fprintf(stderr,"Goodbye - %d was selected\n", abc_pick); */
          *ipick = (long int) abc_pick;
          break;
       case GTK_RESPONSE_CANCEL:
       /*   fprintf(stderr,"Goodbye - %d with original choice\n", abc_pick); */
          *ipick = (long int) abc_pick;
          break;
       case GTK_RESPONSE_HELP:
       /*   fprintf(stderr,"Goodbye - %d help was selected\n", abc_pick); */
          *ipick = -8;
          break;
       case 1:
       /*   fprintf(stderr,"Goodbye - %d default 1 was selected\n", abc_pick); */
          *ipick = 1;
          break;
       case 2:
       /*   fprintf(stderr,"Goodbye - %d default 2 was selected\n", abc_pick); */
          *ipick = 2;
          break;
       default:
          *ipick = -1;
          break;
      }
   gtk_widget_destroy (askbox);

}

