# geometry of bed1 defined in: ../zones/bed1.geo
GEN  bed1  bed1 describes a 1st level north facing room  # type, name, descr
      14       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.00000     7.00000     2.20000  # vert   1
      0.00000     7.00000     2.20000  # vert   2
      0.00000     4.00000     2.20000  # vert   3
      3.00000     4.00000     2.20000  # vert   4
      3.00000     7.00000     4.40000  # vert   5
      0.00000     7.00000     4.40000  # vert   6
      0.00000     4.00000     4.40000  # vert   7
      3.00000     4.00000     4.40000  # vert   8
      3.00000     5.00000     2.20000  # vert   9
      3.00000     5.00000     4.40000  # vert  10
      2.00000     7.00000     3.00000  # vert  11
      0.80000     7.00000     3.00000  # vert  12
      0.80000     7.00000     4.00000  # vert  13
      2.00000     7.00000     4.00000  # vert  14
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1, 11, 14, 13, 12, 11,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  9,  1,  5, 10,
   5,  5,  6,  7,  8, 10,
   5,  4,  3,  2,  1,  9,
   4,  4,  9, 10,  8,
   4, 11, 12, 13, 14,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, north         OPAQ  VERT  ext_wall     EXTERIOR       
  2, west          OPAQ  VERT  ext_part     SIMILAR        
  3, bed2          OPAQ  VERT  int_part     bed2           
  4, bathrm        OPAQ  VERT  int_part     bathrm         
  5, loft          OPAQ  CEIL  loft_ceil    loft           
  6, kitchn        OPAQ  FLOR  upper_flor   kitchen        
  7, door_hall2    OPAQ  VERT  int_doors    hall_2         
  8, window        TRAN  VERT  dbl_glz      EXTERIOR       
# base
   6   0   0   0     9.00
