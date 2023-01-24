
/* Common block definitions as structs for direct access from C code.
   *** Note: definitions must be in _exactly_ the same order as in the
   ***  fortran code. In the code below the struct have been renamed
   ***  slightly so they are disconnected from the fortran commons.
   *** Works ok for 32 bit computers in acting as a bridge between common
   *** blocks and c struct. On some 64 bit computers it is not robust.

   MCOM must equal the value set in building.h <<can this be done automatically??>>
*/
#define MCOM 82

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
  ITVIS visual entity
  ITVOBJ visual object
  ITHLS highlight toggle: normal 0, constr 1, trans/opaq 2, part atrib 3
  ITGRD grid toggle: display = 0, hidden = 1
  ITVNO vertex toggle: display = 0, hidden = 1
  ITPPSW current view - perspective/plan/south/west
  COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &        ITOBS,ITVIS,ITVOBJ,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
*/

struct {
  long int ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,ITOBS,ITVIS,ITVOBJ,ITHLS,ITHLZ,ITGRD;
  float GRDIS;
  long int ITPPSW;
} cray2_;

/*
  COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
*/
struct {
  long int IMT;
  float EYEM[3],VIEWM[3],HITH,YON,ANG,HANG,WIDE;
} cimage_;

/*
  COMMON/C1/NCOMP,NCON
*/
struct {
  long int NCOMP,NCON;
} cc1_;

/*
  COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
*/
struct {
  long int izgfoc,nzg,nznog[MCOM];
} cgzonpik_;
