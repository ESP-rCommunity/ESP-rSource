# geometry of test_cell defined in: ../zones/test_cell.geo
GEN  test_cell  Test Cell  # type, name, descr
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
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface 
   0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  intern_wall  CONSTANT       
  2, Surf-2        OPAQ  VERT  intern_wall  CONSTANT       
  3, Surf-3        OPAQ  VERT  intern_wall  CONSTANT       
  4, Surf-4        OPAQ  VERT  intern_wall  CONSTANT       
  5, Surf-5        OPAQ  CEIL  intern_wall  CONSTANT       
  6, Surf-6        OPAQ  FLOR  intern_wall  CONSTANT       
# base
   6   0   0   0     1.00
