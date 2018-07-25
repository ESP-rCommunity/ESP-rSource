# geometry of mech-room defined in: ../zones/mech-room.geo
GEN  mech-room  Mechanical room  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      3.00000     0.00000     0.00000  # vert   2
      3.00000     3.00000     0.00000  # vert   3
      0.00000     3.00000     0.00000  # vert   4
      0.00000     0.00000     2.00000  # vert   5
      3.00000     0.00000     2.00000  # vert   6
      3.00000     3.00000     2.00000  # vert   7
      0.00000     3.00000     2.00000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Wall-1        OPAQ  VERT  conc_block   EXTERIOR       
  2, Wall-2        OPAQ  VERT  conc_block   EXTERIOR       
  3, Wall-3        OPAQ  VERT  conc_block   EXTERIOR       
  4, Wall-4        OPAQ  VERT  conc_block   EXTERIOR       
  5, Top-5         OPAQ  CEIL  conc_block   EXTERIOR       
  6, Base-6        OPAQ  FLOR  conc_block   EXTERIOR       
# base
  6  0  0  0  0  0     9.00 0
