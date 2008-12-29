# geometry of 300 defined in: 300.geo
GEN  300  300 describes a...  # type, name, descr
      18       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      8.00000     0.00000     0.00000  # vert   2
      0.00000     0.00000     2.70000  # vert   3
      8.00000     0.00000     2.70000  # vert   4
      0.00000     6.00000     0.00000  # vert   5
      8.00000     6.00000     0.00000  # vert   6
      0.00000     6.00000     2.70000  # vert   7
      8.00000     6.00000     2.70000  # vert   8
      4.00000     0.00000     2.70000  # vert   9
      4.00000     0.00000     0.00000  # vert  10
      8.00000     1.50000     0.20000  # vert  11
      8.00000     4.50000     0.20000  # vert  12
      8.00000     4.50000     2.20000  # vert  13
      8.00000     1.50000     2.20000  # vert  14
      0.00000     4.50000     0.20000  # vert  15
      0.00000     1.50000     0.20000  # vert  16
      0.00000     1.50000     2.20000  # vert  17
      0.00000     4.50000     2.20000  # vert  18
# no of vertices followed by list of associated vert
  10,  2,  6,  8,  4,  2, 11, 14, 13, 12, 11,
   4,  6,  5,  7,  8,
  10,  5,  1,  3,  7,  5, 15, 18, 17, 16, 15,
   5,  3,  9,  4,  8,  7,
   5,  5,  6,  2, 10,  1,
   6,  1, 10,  2,  4,  9,  3,
   4, 11, 12, 13, 14,
   4, 15, 16, 17, 18,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   5   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, East_Wall     OPAQ  VERT  LWextwall6   EXTERIOR       
  2, North_Wall    OPAQ  VERT  LWextwall6   EXTERIOR       
  3, West_Wall     OPAQ  VERT  LWextwall6   EXTERIOR       
  4, Roof          OPAQ  CEIL  LWroof6      EXTERIOR       
  5, Floor         OPAQ  FLOR  LWfloor6     GROUND         
  6, South_Wall    OPAQ  VERT  LWextwall6   EXTERIOR       
  7, EastWindow    TRAN  VERT  dg_glazing   EXTERIOR       
  8, WestWindow    TRAN  VERT  dg_glazing   EXTERIOR       
# base
  5  0  0  0  0  0    48.00
