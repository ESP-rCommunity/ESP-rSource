# geometry of bed2 defined in: ../zone/bed2.geo
GEN  bed2  bed2 describes a...  # type, name, descr
      14       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.75100    12.59100     0.00000  # vert   1
     10.30100    15.27600     0.00000  # vert   2
      7.61700    16.82600     0.00000  # vert   3
      6.06700    14.14100     0.00000  # vert   4
      8.75100    12.59100     2.40000  # vert   5
     10.30100    15.27600     2.40000  # vert   6
      7.61700    16.82600     2.40000  # vert   7
      6.06700    14.14100     2.40000  # vert   8
      9.60800    15.67600     0.60000  # vert   9
      8.87200    16.10100     0.60000  # vert  10
      8.87200    16.10100     2.10000  # vert  11
      9.60800    15.67600     2.10000  # vert  12
      7.97200    13.04100     0.00000  # vert  13
      7.97200    13.04100     2.40000  # vert  14
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
  10,  2,  3,  7,  6,  2,  9, 12, 11, 10,  9,
   4,  3,  4,  8,  7,
   4,  4, 13, 14,  8,
   5,  5,  6,  7,  8, 14,
   5,  1, 13,  4,  3,  2,
   4,  9, 10, 11, 12,
   4, 13,  1,  5, 14,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, s_bed1        OPAQ  VERT  int_buf      bed1           
  2, ext_east      OPAQ  VERT  ext_east     EXTERIOR       
  3, s_adjhouse    OPAQ  VERT  ext_west     SIMILAR        
  4, s_bath        OPAQ  VERT  int_buf      bath           
  5, ceil          OPAQ  CEIL  int_roof     roof           
  6, floor         OPAQ  FLOR  house_floor  GROUND         
  7, win           TRAN  VERT  doub_glaz    EXTERIOR       
  8, s_hall        OPAQ  VERT  door         hall           
# base
  6  0  0  0  0  0     9.61
