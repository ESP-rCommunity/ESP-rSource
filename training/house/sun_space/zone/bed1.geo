# geometry of bed1 defined in: ../zone/bed1.geo
GEN  bed1  bed1 describes a...  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     11.43600    11.04100     0.00000  # vert   1
     12.98600    13.72600     0.00000  # vert   2
     10.30100    15.27600     0.00000  # vert   3
      8.75100    12.59100     0.00000  # vert   4
     11.43600    11.04100     2.40000  # vert   5
     12.98600    13.72600     2.40000  # vert   6
     10.30100    15.27600     2.40000  # vert   7
      8.75100    12.59100     2.40000  # vert   8
     11.94700    14.32600     0.60000  # vert   9
     11.21000    14.75100     0.60000  # vert  10
     11.21000    14.75100     2.10000  # vert  11
     11.94700    14.32600     2.10000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
  10,  2,  3,  7,  6,  2,  9, 12, 11, 10,  9,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, s_buf         OPAQ  VERT  int_buf      buf_2          
  2, ext_east      OPAQ  VERT  ext_east     EXTERIOR       
  3, s_bed2        OPAQ  VERT  int_buf      bed2           
  4, s_hall        OPAQ  VERT  int_buf      hall           
  5, ceil          OPAQ  CEIL  int_roof     roof           
  6, floor         OPAQ  FLOR  house_floor  GROUND         
  7, win           TRAN  VERT  doub_glaz    EXTERIOR       
# base
  6  0  0  0  0  0     9.61
