# geometry of Support2 defined in: ../zones/Support2.geo
GEN  Support2  Support2 holds the PV module(s)  # type, name, descr
      10       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      2.00000     0.00000     0.00000  # vert   1
      2.00000     3.00000     0.00000  # vert   2
      4.00000     3.00000     0.00000  # vert   3
      4.00000     0.00000     0.00000  # vert   4
      2.00000     0.00000     1.26800  # vert   5
      2.00000     3.00000     3.00000  # vert   6
      4.00000     3.00000     3.00000  # vert   7
      4.00000     0.00000     1.26800  # vert   8
      2.00000     1.00000     3.00000  # vert   9
      4.00000     1.00000     3.00000  # vert  10
# no of vertices followed by list of associated vert
   5,  1,  5,  9,  6,  2,
   4,  2,  6,  7,  3,
   5,  4,  3,  7, 10,  8,
   4,  4,  8,  5,  1,
   4,  9, 10,  7,  6,
   4,  1,  2,  3,  4,
   4,  5,  8, 10,  9,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, WWall         OPAQ  VERT  extern_wall  EXTERIOR       
  2, NWall         OPAQ  VERT  extern_wall  EXTERIOR       
  3, EWall         OPAQ  VERT  extern_wall  EXTERIOR       
  4, SWall         OPAQ  VERT  extern_wall  EXTERIOR       
  5, Ceiling       OPAQ  CEIL  extern_wall  EXTERIOR       
  6, Floor         OPAQ  FLOR  floor_1      GROUND         
  7, PVModule      TRAN  SLOP  PVmod_s      EXTERIOR       
# base
  6  5  7  0  0  0    14.00
