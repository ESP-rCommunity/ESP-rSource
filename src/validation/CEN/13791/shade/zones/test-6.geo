# geometry of test-6 defined in: zones/test-6.geo
GEN  test-6  test-6 describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     15.00000    12.00000     0.00000  # vert   1
     20.00000    12.00000     0.00000  # vert   2
     20.00000    17.00000     0.00000  # vert   3
     15.00000    17.00000     0.00000  # vert   4
     15.00000    12.00000     5.00000  # vert   5
     20.00000    12.00000     5.00000  # vert   6
     20.00000    17.00000     5.00000  # vert   7
     15.00000    17.00000     5.00000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  extern_wall  EXTERIOR       
  6, Surf-6        OPAQ  FLOR  extern_wall  EXTERIOR       
# base
  6  0  0  0  0  0    25.00 0
