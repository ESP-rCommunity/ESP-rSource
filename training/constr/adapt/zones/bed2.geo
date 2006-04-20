# geometry of bed2 defined in: ../zones/bed2.geo
GEN  bed2  bed2 is a south facing bedroom over the lounge  # type, name, descr
      15       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.00000     3.00000     2.20000  # vert   1
      3.00000     4.00000     2.20000  # vert   2
      0.00000     4.00000     2.20000  # vert   3
      0.00000     0.00000     2.20000  # vert   4
      3.00000     0.00000     2.20000  # vert   5
      3.00000     3.00000     4.40000  # vert   6
      3.00000     4.00000     4.40000  # vert   7
      0.00000     4.00000     4.40000  # vert   8
      0.00000     0.00000     4.40000  # vert   9
      3.00000     0.00000     4.40000  # vert  10
      0.00000     3.00000     2.20000  # vert  11
      1.00000     0.00000     3.20000  # vert  12
      2.00000     0.00000     3.20000  # vert  13
      2.00000     0.00000     4.20000  # vert  14
      1.00000     0.00000     4.20000  # vert  15
# no of vertices followed by list of associated vert
   4,  1,  2,  7,  6,
   4,  2,  3,  8,  7,
   5,  3, 11,  4,  9,  8,
  10,  4,  5, 10,  9,  4, 12, 15, 14, 13, 12,
   4,  5,  1,  6, 10,
   5,  7,  8,  9, 10,  6,
   4,  1,  5,  4, 11,
   4,  1, 11,  3,  2,
   4, 12, 13, 14, 15,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, door_hall2    OPAQ  VERT  int_doors    hall_2         
  2, bed1          OPAQ  VERT  int_part     bed1           
  3, west          OPAQ  VERT  ext_part     SIMILAR        
  4, south         OPAQ  VERT  ext_wall     EXTERIOR       
  5, bed3          OPAQ  VERT  int_part     bed3           
  6, loft          OPAQ  CEIL  loft_ceil    loft           
  7, living        OPAQ  FLOR  upper_flor   liv_room       
  8, kitch         OPAQ  FLOR  upper_flor   kitchen        
  9, window        TRAN  VERT  dbl_glz      EXTERIOR       
# base
   7   8   0   0    12.00
