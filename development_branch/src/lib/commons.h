
/* Common block definitions as structs for direct access from C code.
   *** Note: definitions must be in _exactly_ the same order as in the
   ***  fortran code.

   MCOM must equal the value set in building.h <<can this be done automatically??>>
*/
#define MCOM 28

/*
  ITDSP labels toggle: all surf + obs = 0, all surf = 1, partn = 2,
        similar = 4, surfs + obs+ ground = 5, ground only = 6
  ITBND bounds toggle: static = 0, optimum = 1, zone focus = 2
  ITEPT is ??
  ITZNM zone name toggle: display = 0, hidden = 1
  ITSNM surface name toggle: display = 0, hidden = 1
  ITORG origin toggle: display = 0, hidden = 1
  ITSNR surf normal toggle: display = 0, hidden = 1.
  ITOBS obstruction toggle: not yet enabled.
  ITHLS highlight toggle: normal 0, constr 1, trans/opaq 2, part atrib 3
  ITGRD grid toggle: display = 0, hidden = 1
  ITVNO vertex toggle: display = 0, hidden = 1
  ITPPSW current view - perspective/plan/south/west
  COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
*/

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


extern struct {
  long int ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,ITOBS,ITHLS,ITHLZ,ITGRD;
  float GRDIS;
  long int ITPPSW;
} ray2_;

/*
  COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
*/
extern struct {
  long int IMT;
  float EYEM[3],VIEWM[3],HITH,YON,ANG,HANG,WIDE;
} image_;

/*
  COMMON/C1/NCOMP,NCON
*/
extern struct {
  long int NCOMP,NCON;
} c1_;

/*
  COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
*/
extern struct {
  long int izgfoc,nzg,nznog[MCOM];
} gzonpik_;
