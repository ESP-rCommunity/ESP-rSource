# geometry of East_part defined in: east_part.geo
GEN  East_part  East_part describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     26.00000     0.00000     0.00000  # vert   1
     26.00000    18.50000     0.00000  # vert   2
     50.50000    18.50000     0.00000  # vert   3
     50.50000     0.00000     0.00000  # vert   4
     26.00000     0.00000    14.95000  # vert   5
     26.00000    18.50000    14.95000  # vert   6
     50.50000    18.50000    14.95000  # vert   7
     50.50000     0.00000    14.95000  # vert   8
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
  1, Surf-1        OPAQ  VERT  Facade       EXTERIOR       
  2, Surf-2        OPAQ  VERT  Facade       EXTERIOR       
  3, Surf-3        OPAQ  VERT  Facade       EXTERIOR       
  4, Surf-4        OPAQ  VERT  Facade       EXTERIOR       
  5, Surf-5        OPAQ  FLOR  roof         EXTERIOR       
  6, Surf-6        OPAQ  CEIL  Base_down    ADIABATIC      
# base
   5   0   0   0   453.25
