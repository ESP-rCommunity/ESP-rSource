# geometry of 440 defined in: 440.geo
GEN  440  440 describes a...  # type, name, descr
      18       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      8.00000     0.00000     0.00000  # vert   2
      0.00000     0.00000     2.70000  # vert   3
      8.00000     0.00000     2.70000  # vert   4
      0.50000     0.00000     0.20000  # vert   5
      3.50000     0.00000     0.20000  # vert   6
      0.50000     0.00000     2.20000  # vert   7
      3.50000     0.00000     2.20000  # vert   8
      4.50000     0.00000     0.20000  # vert   9
      7.50000     0.00000     0.20000  # vert  10
      4.50000     0.00000     2.20000  # vert  11
      7.50000     0.00000     2.20000  # vert  12
      0.00000     6.00000     0.00000  # vert  13
      8.00000     6.00000     0.00000  # vert  14
      0.00000     6.00000     2.70000  # vert  15
      8.00000     6.00000     2.70000  # vert  16
      4.00000     0.00000     2.70000  # vert  17
      4.00000     0.00000     0.00000  # vert  18
# no of vertices followed by list of associated vert
  10,  1, 18, 17,  8,  6,  5,  7,  8, 17,  3,
  10, 18,  2,  4, 12, 10,  9, 11, 12,  4, 17,
   4,  2, 14, 16,  4,
   4, 14, 13, 15, 16,
   4, 13,  1,  3, 15,
   4,  5,  6,  8,  7,
   4,  9, 10, 12, 11,
   5,  3, 17,  4, 16, 15,
   5, 13, 14,  2, 18,  1,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   9   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, South_Wall    OPAQ  VERT  LWextwall8   EXTERIOR       
  2, South_Wall2   OPAQ  VERT  LWextwall8   EXTERIOR       
  3, East_Wall     OPAQ  VERT  LWextwall8   EXTERIOR       
  4, North_Wall    OPAQ  VERT  LWextwall8   EXTERIOR       
  5, West_Wall     OPAQ  VERT  LWextwall8   EXTERIOR       
  6, Window        TRAN  VERT  dg_glazing   EXTERIOR       
  7, Window2       TRAN  VERT  dg_glazing   EXTERIOR       
  8, Roof          OPAQ  CEIL  LWroof8      EXTERIOR       
  9, Floor         OPAQ  FLOR  LWfloor3     GROUND         
# base
  9  0  0  0  0  0    48.00
