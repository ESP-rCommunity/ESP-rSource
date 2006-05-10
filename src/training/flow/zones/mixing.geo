# geometry of mixing defined in: ../zones/mixing.geo
GEN  mixing  mixing describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.10000  # vert   1
      0.10000     0.00000     0.10000  # vert   2
      0.10000     0.10000     0.10000  # vert   3
      0.00000     0.10000     0.10000  # vert   4
      0.00000     0.00000     0.20000  # vert   5
      0.10000     0.00000     0.20000  # vert   6
      0.10000     0.10000     0.20000  # vert   7
      0.00000     0.10000     0.20000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface 
  0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  extern_wall  EXTERIOR       
  6, Surf-6        OPAQ  FLOR  extern_wall  EXTERIOR       
# base
   6   0   0   0     0.01
