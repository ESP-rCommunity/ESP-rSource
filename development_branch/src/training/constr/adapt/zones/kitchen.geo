# geometry of kitchen defined in: ../zones/kitchen.geo
GEN  kitchen  kitchen describes a room adjacent to living and hall  # type, name, descr
      17      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.00000     7.00000     0.00000  # vert   1
      0.00000     7.00000     0.00000  # vert   2
      0.00000     3.00000     0.00000  # vert   3
      3.00000     3.00000     0.00000  # vert   4
      3.00000     7.00000     2.20000  # vert   5
      0.00000     7.00000     2.20000  # vert   6
      0.00000     3.00000     2.20000  # vert   7
      3.00000     3.00000     2.20000  # vert   8
      3.00000     4.00000     2.20000  # vert   9
      0.00000     4.00000     2.20000  # vert  10
      3.00000     5.50000     2.20000  # vert  11
      3.00000     5.50000     0.00000  # vert  12
      2.00000     7.00000     0.80000  # vert  13
      0.80000     7.00000     0.80000  # vert  14
      0.80000     7.00000     2.00000  # vert  15
      2.00000     7.00000     2.00000  # vert  16
      3.00000     4.00000     0.00000  # vert  17
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1, 13, 16, 15, 14, 13,
   5,  2,  3,  7, 10,  6,
   4,  3,  4,  8,  7,
   4, 17, 12, 11,  9,
   5,  5,  6, 10,  9, 11,
   6,  4,  3,  2,  1, 12, 17,
   4,  9, 10,  7,  8,
   4, 12,  1,  5, 11,
   4, 13, 14, 15, 16,
   4,  4, 17,  9,  8,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, north         OPAQ  VERT  ext_wall     EXTERIOR       
  2, west          OPAQ  VERT  ext_part     SIMILAR        
  3, living        OPAQ  VERT  int_part     liv_room       
  4, hall1         OPAQ  VERT  int_part     hall_1         
  5, bed1          OPAQ  CEIL  ceiling      bed1           
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, bed2          OPAQ  CEIL  ceiling      bed2           
  8, east          OPAQ  VERT  ext_wall     EXTERIOR       
  9, window        TRAN  VERT  dbl_glz      EXTERIOR       
 10, door_hall1    OPAQ  VERT  int_doors    hall_1         
# base
   6   0   0   0    12.00
