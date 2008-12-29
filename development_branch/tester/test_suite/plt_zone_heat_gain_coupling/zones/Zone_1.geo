# geometry of Zone_1 defined in: ../zones/Zone_1.geo
GEN  Zone_1  Zone_1 describes a single adiabatic zone  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      3.00000     0.00000     0.00000  # vert   2
      3.00000     4.00000     0.00000  # vert   3
      0.00000     4.00000     0.00000  # vert   4
      0.00000     0.00000     2.70000  # vert   5
      3.00000     0.00000     2.70000  # vert   6
      3.00000     4.00000     2.70000  # vert   7
      0.00000     4.00000     2.70000  # vert   8
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
  1, Surf-1        OPAQ  VERT  int_part     ADIABATIC      
  2, Surf-2        OPAQ  VERT  int_part     ADIABATIC      
  3, Surf-3        OPAQ  VERT  int_part     ADIABATIC      
  4, Surf-4        OPAQ  VERT  int_part     Zone_2         
  5, Surf-5        OPAQ  CEIL  int_part     CONSTANT       
  6, Surf-6        OPAQ  FLOR  int_part     ADIABATIC      
# base
  6  0  0  0  0  0    12.00 0
