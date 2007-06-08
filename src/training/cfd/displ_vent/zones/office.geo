# geometry of office defined in: ../zones/office.geo
GEN  office  office describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      1.00000     5.00000     0.00000  # vert   1
      5.00000     5.00000     0.00000  # vert   2
      5.00000     9.00000     0.00000  # vert   3
      1.00000     9.00000     0.00000  # vert   4
      1.00000     5.00000     3.00000  # vert   5
      5.00000     5.00000     3.00000  # vert   6
      5.00000     9.00000     3.00000  # vert   7
      1.00000     9.00000     3.00000  # vert   8
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
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, part_b        OPAQ  VERT  int_part     reception      
  2, part_a        OPAQ  VERT  int_part     reception      
  3, North_w       OPAQ  VERT  extern_wall  EXTERIOR       
  4, West_w        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Ceiling       OPAQ  CEIL  ceiling      roof_space     
  6, Floor         OPAQ  FLOR  floor_1      GROUND         
# base
  0  0  0  0  0  0    16.00 1
