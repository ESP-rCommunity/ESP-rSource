# geometry of 960_sun defined in: ../960/960_sun.geo
GEN  960_sun  960_sun describes a...  # type, name, descr
      16       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000    -2.00000     0.00000  # vert   1
      8.00000    -2.00000     0.00000  # vert   2
      8.00000     0.00000     0.00000  # vert   3
      0.00000     0.00000     0.00000  # vert   4
      0.00000    -2.00000     2.70000  # vert   5
      8.00000    -2.00000     2.70000  # vert   6
      8.00000     0.00000     2.70000  # vert   7
      0.00000     0.00000     2.70000  # vert   8
      0.50000    -2.00000     0.50000  # vert   9
      3.50000    -2.00000     0.50000  # vert  10
      3.50000    -2.00000     2.50000  # vert  11
      0.50000    -2.00000     2.50000  # vert  12
      4.50000    -2.00000     0.50000  # vert  13
      7.50000    -2.00000     0.50000  # vert  14
      7.50000    -2.00000     2.50000  # vert  15
      4.50000    -2.00000     2.50000  # vert  16
# no of vertices followed by list of associated vert
  16,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,  1, 13, 16, 15, 14, 13,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, South         OPAQ  VERT  HW_extwall   EXTERIOR       
  2, East          OPAQ  VERT  HW_extwall   EXTERIOR       
  3, CommonWall    OPAQ  VERT  CommonWalll  960_back       
  4, West          OPAQ  VERT  HW_extwall   EXTERIOR       
  5, Roof          OPAQ  CEIL  HWroof       EXTERIOR       
  6, Floor         OPAQ  FLOR  HWfloor      GROUND         
  7, Window        TRAN  VERT  dg_glazing   EXTERIOR       
  8, Window2       TRAN  VERT  dg_glazing   EXTERIOR       
# base
  6  0  0  0  0  0    16.00
