# geometry of bath defined in: ../zone/bath.geo
GEN  bath  bath describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      7.12200    11.57000     0.00000  # vert   1
      7.97200    13.04200     0.00000  # vert   2
      6.06600    14.14200     0.00000  # vert   3
      5.21700    12.67000     0.00000  # vert   4
      7.12200    11.57000     2.40000  # vert   5
      7.97200    13.04200     2.40000  # vert   6
      6.06600    14.14200     2.40000  # vert   7
      5.21700    12.67000     2.40000  # vert   8
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
  1, s_hall        OPAQ  VERT  int_buf      hall           
  2, s_bed2        OPAQ  VERT  int_buf      bed2           
  3, s_adjhouse    OPAQ  VERT  ext_west     SIMILAR        
  4, s_liv         OPAQ  VERT  int_buf      kitliv         
  5, ceil          OPAQ  CEIL  int_roof     roof           
  6, floor         OPAQ  FLOR  house_floor  GROUND         
# base
  6  0  0  0  0  0     3.74
