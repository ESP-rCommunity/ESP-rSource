# geometry of office defined in: ../zone/office.geo
GEN  office                 # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      1.000     5.000     0.000  # vert  1
      5.000     5.000     0.000  # vert  2
      5.000     9.000     0.000  # vert  3
      1.000     9.000     0.000  # vert  4
      1.000     5.000     3.000  # vert  5
      5.000     5.000     3.000  # vert  6
      5.000     9.000     3.000  # vert  7
      1.000     9.000     3.000  # vert  8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, part_b        OPAQ  VERT  int_part     reception      
  2, part_a        OPAQ  VERT  int_part     reception      
  3, North_w       OPAQ  VERT  extern_wall  EXTERIOR       
  4, West_w        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Ceiling       OPAQ  CEIL  ceiling      roof_space     
  6, Floor         OPAQ  FLOR  floor_1      GROUND         
