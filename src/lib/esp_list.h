#ifndef __ESP_LIST_H__
#define __ESP_LIST_H__

#include <stdio.h>
#include <gtk/gtk.h>

#ifdef ESP_LIST_MAIN
 gchar *esp_selected;	/* selected dynamic menu item */
 gint got_item_n;
#else
 extern gchar *esp_selected;
 extern gint  got_item_n;
#endif

#endif
