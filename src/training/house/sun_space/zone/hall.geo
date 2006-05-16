# geometry of hall defined in: ../zone/hall.geo
GEN  hall  hall describes a...  # type, name, descr
      14       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.58600     9.56900     0.00000  # vert   1
     11.43600    11.04100     0.00000  # vert   2
      7.97200    13.04200     0.00000  # vert   3
      7.12300    11.56900     0.00000  # vert   4
     10.58600     9.56900     2.40000  # vert   5
     11.43600    11.04100     2.40000  # vert   6
      7.97200    13.04200     2.40000  # vert   7
      7.12300    11.56900     2.40000  # vert   8
     10.93600    10.17500     0.00000  # vert   9
     11.33600    10.86800     0.00000  # vert  10
     11.33600    10.86800     2.10000  # vert  11
     10.93600    10.17500     2.10000  # vert  12
      8.75100    12.59100     0.00000  # vert  13
      8.75100    12.59100     2.40000  # vert  14
# no of vertices followed by list of associated vert
   8,  1,  9, 12, 11, 10,  2,  6,  5,
   4,  2, 13, 14,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   5,  5,  6, 14,  7,  8,
   7,  1,  4,  3, 13,  2, 10,  9,
   4,  9, 10, 11, 12,
   4, 13,  3,  7, 14,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, s_buf         OPAQ  VERT  int_buf      buf_2          
  2, s_bed1        OPAQ  VERT  int_buf      bed1           
  3, s_bath        OPAQ  VERT  int_buf      bath           
  4, s_liv         OPAQ  VERT  int_buf      kitliv         
  5, ceil          OPAQ  CEIL  int_roof     roof           
  6, floor         OPAQ  FLOR  house_floor  GROUND         
  7, door          OPAQ  VERT  door         buf_2          
  8, s_bed2        OPAQ  VERT  door         bed2           
# base
  6  0  0  0  0  0     6.80
