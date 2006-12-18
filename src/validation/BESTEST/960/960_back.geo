# geometry of 960_back defined in: ../960/960_back.geo
GEN  960_back  960_back describes a...  # type, name, descr
      10       6   0.000    # vertices, surfaces, rotation angle
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
# no of vertices followed by list of associated vert
   4,  2,  6,  8,  4,
   4,  6,  5,  7,  8,
   4,  5,  1,  3,  7,
   5,  3,  9,  4,  8,  7,
   5,  5,  6,  2, 10,  1,
   6,  1, 10,  2,  4,  9,  3,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    1   5   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, East_Wall     OPAQ  VERT  LWextwall7   EXTERIOR       
  2, North_Wall    OPAQ  VERT  LWextwall7   EXTERIOR       
  3, West_Wall     OPAQ  VERT  LWextwall7   EXTERIOR       
  4, Roof          OPAQ  CEIL  LWroof7      EXTERIOR       
  5, Floor         OPAQ  FLOR  LWfloor7     GROUND         
  6, CommonWall    OPAQ  VERT  CommonWalll  960_sun        
# base
  5  0  0  0  0  0    48.00
