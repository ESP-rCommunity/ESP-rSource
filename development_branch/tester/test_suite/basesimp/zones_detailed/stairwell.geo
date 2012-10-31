# geometry of stairwell defined in: ../zones/stairwell.geo
GEN  stairwell  stairwell describes the stairwell for the basement will be merg  # type, name, descr
      20      15   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.83000     0.00000     0.00000  # vert   1
      7.25000     0.00000     0.00000  # vert   2
      7.25000     2.27000     0.00000  # vert   3
      4.83000     2.27000     0.00000  # vert   4
      4.83000     0.00000     2.60000  # vert   5
      7.25000     0.00000     2.60000  # vert   6
      7.25000     2.27000     2.60000  # vert   7
      4.83000     2.27000     2.60000  # vert   8
      7.25000     0.00000     5.62000  # vert   9
      4.83000     0.00000     5.62000  # vert  10
      7.25000     2.27000     5.62000  # vert  11
      4.83000     2.27000     5.62000  # vert  12
      7.25000     0.00000     8.06500  # vert  13
      4.83000     0.00000     8.06500  # vert  14
      7.25000     2.27000     8.06500  # vert  15
      4.83000     2.27000     8.06500  # vert  16
      5.37500     0.00000     5.72000  # vert  17
      6.62500     0.00000     5.72000  # vert  18
      6.62500     0.00000     6.97000  # vert  19
      5.37500     0.00000     6.97000  # vert  20
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  1,  4,  3,  2,
   4,  7,  8, 12, 11,
   4,  8,  5, 10, 12,
   4,  5,  6,  9, 10,
  10, 10,  9, 13, 14, 10, 17, 18, 19, 20, 17,
   4, 15, 11, 12, 16,
   4, 14, 16, 12, 10,
   4, 15, 13,  9, 11,
   4, 14, 13, 15, 16,
   4, 11,  9,  6,  7,
   4, 18, 17, 20, 19,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, st_bsm_out    OPAQ  VERT  foundation_  BASESIMP       
  2, bsm_south     OPAQ  VERT  int_partitn  basement       
  3, bsm_east      TRAN  VERT  fictitious   basement       
  4, bsm_north     OPAQ  VERT  int_partitn  basement       
  5, stair_floor   OPAQ  FLOR  slab_floor   BASESIMP       
  6, main_east     TRAN  VERT  fictitious   mainfloor      
  7, main_north    OPAQ  VERT  int_partitn  mainfloor      
  8, st_1_out      OPAQ  VERT  ccht_wall    EXTERIOR       
  9, st_2_out      OPAQ  VERT  ccht_wall    EXTERIOR       
 10, 2nd_east      TRAN  VERT  fictitious   secondfloor    
 11, 2nd_north     OPAQ  VERT  int_partitn  secondfloor    
 12, 2nd_south     OPAQ  VERT  int_partitn  secondfloor    
 13, stair_ceilin  OPAQ  CEIL  ceiling      roof           
 14, main_south    OPAQ  VERT  int_partitn  mainfloor      
 15, w_triangle    TRAN  VERT  ccht_window  EXTERIOR       
# base
   5   0   0   0   0   0     5.49
