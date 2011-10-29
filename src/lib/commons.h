
/* Create an enum which will point to the Indices of the array called
 * font_calculations_array[] 
 */
  enum{
    serif_small,      /* 0 position in array is for Serif Small */
    serif_medium,     /* 1 position in array is for Serif Medium */
    serif_large,      /* 2 position in array is for Serif Large */
    serif_largest,    /* 3 position in array is for Serif Largest */
    courier_small,    /* 4 position in array is for Courier Small */
    courier_medium,   /* 5 position in array is for Courier Medium */
    courier_large,    /* 6 position in array is for Courier Large */
    courier_largest}; /* 7 position in array is for Courier Largest */

/* Create a structure called Font_Calculation_Variables that
 * contains the variables needed for font calculations
 */
typedef struct  {

   gint f_height;     // pixel height of default font 
   gint f_width;      // pixel width of default font 
 }Font_Calculation_Variables;
 
 
/* Create an aray of Font_Calculation_Variables */

Font_Calculation_Variables font_calculations_array[8]; 
