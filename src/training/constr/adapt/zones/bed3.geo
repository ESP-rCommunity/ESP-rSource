# geometry of bed3 defined in: ../zones/bed3.geo
GEN  bed3  bed3 describes a...  # type, name, descr
      14       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.00000     3.00000     2.20000  # vert   1
      3.00000     3.00000     2.20000  # vert   2
      3.00000     0.00000     2.20000  # vert   3
      6.00000     0.00000     2.20000  # vert   4
      6.00000     3.00000     4.40000  # vert   5
      3.00000     3.00000     4.40000  # vert   6
      3.00000     0.00000     4.40000  # vert   7
      6.00000     0.00000     4.40000  # vert   8
      4.00000     0.00000     3.20000  # vert   9
      5.00000     0.00000     3.20000  # vert  10
      5.00000     0.00000     4.20000  # vert  11
      4.00000     0.00000     4.20000  # vert  12
      4.00000     3.00000     2.20000  # vert  13
      4.00000     3.00000     4.40000  # vert  14
# no of vertices followed by list of associated vert
   4,  1, 13, 14,  5,
   4,  2,  3,  7,  6,
  10,  3,  4,  8,  7,  3,  9, 12, 11, 10,  9,
   4,  4,  1,  5,  8,
   5,  5, 14,  6,  7,  8,
   5,  1,  4,  3,  2, 13,
   4,  9, 10, 11, 12,
   4, 13,  2,  6, 14,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, hall2         OPAQ  VERT  int_part     hall_2         
  2, bed2          OPAQ  VERT  int_part     bed2           
  3, south         OPAQ  VERT  ext_wall     EXTERIOR       
  4, east          OPAQ  VERT  ext_part     SIMILAR        
  5, loft          OPAQ  CEIL  loft_ceil    loft           
  6, living        OPAQ  FLOR  upper_flor   liv_room       
  7, window        TRAN  VERT  dbl_glz      EXTERIOR       
  8, door_hall2    OPAQ  VERT  int_doors    hall_2         
# base
   6   0   0   0     9.00
