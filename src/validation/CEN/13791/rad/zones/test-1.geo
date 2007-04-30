# geometry of test-1 defined in: zones/test-1.geo
GEN  test-1  test-1 describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      1.00000     0.00000     0.00000  # vert   2
      1.00000     1.00000     0.00000  # vert   3
      0.00000     1.00000     0.00000  # vert   4
      0.00000     0.00000     1.00000  # vert   5
      1.00000     0.00000     1.00000  # vert   6
      1.00000     1.00000     1.00000  # vert   7
      0.00000     1.00000     1.00000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  4,  1,  5,  8,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  cond_1.0     CONSTANT       
  2, Surf-2        OPAQ  VERT  cond_5.0     CONSTANT       
  3, Surf-3b       OPAQ  VERT  cond_1.0     CONSTANT       
  4, Surf-3a       OPAQ  VERT  cond_1.0     CONSTANT       
  5, Surf-4        OPAQ  CEIL  cond_1.0     CONSTANT       
  6, Surf-5        OPAQ  FLOR  cond_1.0     CONSTANT       
# base
  6  0  0  0  0  0     1.00 0
