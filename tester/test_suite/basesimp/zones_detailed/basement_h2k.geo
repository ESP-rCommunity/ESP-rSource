# geometry of basement defined in: ../zones/basement_h2k.geo
GEN  basement  basement describes the basement not including the area below the  # type, name, descr
      20      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
     12.00000     0.00000     0.00000  # vert   2
     12.00000     6.79000     0.00000  # vert   3
      4.97000     6.79000     0.00000  # vert   4
      4.97000    12.20000     0.00000  # vert   5
      0.00000    12.20000     0.00000  # vert   6
      0.00000     0.00000     2.60000  # vert   7
     12.00000     0.00000     2.60000  # vert   8
     12.00000     6.79000     2.60000  # vert   9
      4.97000     6.79000     2.60000  # vert  10
      4.97000    12.20000     2.60000  # vert  11
      0.00000    12.20000     2.60000  # vert  12
      4.83000     0.00000     2.60000  # vert  13
      4.83000     2.27000     0.00000  # vert  14
      4.83000     2.27000     2.60000  # vert  15
      7.25000     2.27000     2.60000  # vert  16
      7.25000     0.00000     2.60000  # vert  17
      4.83000     0.00000     0.00000  # vert  18
      7.25000     0.00000     0.00000  # vert  19
      7.25000     2.27000     0.00000  # vert  20
# no of vertices followed by list of associated vert
   4,  3,  4, 10,  9,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
  10,  7, 13, 15, 16, 17,  8,  9, 10, 11, 12,
  10,  1,  6,  5,  4,  3,  2, 19, 20, 14, 18,
   4, 13, 18, 14, 15,
   4, 16, 15, 14, 20,
   4, 17, 16, 20, 19,
   4, 19,  2,  8, 17,
   4,  1, 18, 13,  7,
   4,  2,  3,  9,  8,
   4,  6,  1,  7, 12,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, found_lft2    OPAQ  VERT  ccht_wall    BASESIMP       
  2, found_frnt2   OPAQ  VERT  foundation_  BASESIMP       
  3, found_lft1    OPAQ  VERT  foundation_  BASESIMP       
  4, bsment_ceiln  OPAQ  CEIL  floors_r     mainfloor      
  5, bsment_floor  OPAQ  FLOR  slab_floor   BASESIMP       
  6, stair_north   OPAQ  VERT  int_partitn  stairwell      
  7, stair_east    TRAN  VERT  fictitious   stairwell      
  8, stair_south   OPAQ  VERT  int_partitn  stairwell      
  9, found_rgt2    OPAQ  VERT  foundation_  BASESIMP       
 10, found_rgt1    OPAQ  VERT  foundation_  BASESIMP       
 11, found_frnt1   OPAQ  VERT  foundation_  BASESIMP       
 12, found_back    OPAQ  VERT  foundation_  BASESIMP       
# base
   5   0   0   0   0   0   102.87
