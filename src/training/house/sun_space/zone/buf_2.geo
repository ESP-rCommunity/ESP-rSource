# geometry of buf_2 defined in: ../zone/buf_2.geo
GEN  buf_2  buf_2 describes a...  # type, name, descr
      18       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     15.49800    12.27600     0.00000  # vert   1
     12.98600    13.72700     0.00000  # vert   2
     15.49800    12.27600     2.40000  # vert   3
     12.98600    13.72700     2.40000  # vert   4
     15.32500    12.37600     0.30000  # vert   5
     13.24700    13.57600     0.30000  # vert   6
     13.24700    13.57600     2.10000  # vert   7
     15.32500    12.37600     2.10000  # vert   8
     10.58600     9.57000     0.00000  # vert   9
     11.43600    11.04200     0.00000  # vert  10
     11.43600    11.04200     2.40000  # vert  11
     10.58600     9.57000     2.40000  # vert  12
     11.33600    10.86800     0.00000  # vert  13
     10.93600    10.17500     0.00000  # vert  14
     10.93600    10.17500     2.10000  # vert  15
     11.33600    10.86800     2.10000  # vert  16
     13.09800     8.12000     0.00000  # vert  17
     13.09800     8.12000     2.40000  # vert  18
# no of vertices followed by list of associated vert
   4, 17,  1,  3, 18,
  10,  1,  2,  4,  3,  1,  5,  8,  7,  6,  5,
   4,  2, 10, 11,  4,
   5, 18,  3,  4, 11, 12,
   7, 17,  9, 14, 13, 10,  2,  1,
   4,  5,  6,  7,  8,
   8, 10, 13, 16, 15, 14,  9, 12, 11,
   4, 13, 14, 15, 16,
   4,  9, 17, 18, 12,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, ext_wall      OPAQ  VERT  ext_buf      EXTERIOR       
  2, east          OPAQ  VERT  ext_west     EXTERIOR       
  3, s_bed         OPAQ  VERT  int_buf      bed1           
  4, fict          TRAN  CEIL  fictitious   buf_roof       
  5, floor         OPAQ  FLOR  buf_floor    GROUND         
  6, eastwin       TRAN  VERT  doub_glaz    EXTERIOR       
  7, s_hall        OPAQ  VERT  int_buf      hall           
  8, door          OPAQ  VERT  door         hall           
  9, fictv         TRAN  VERT  fictitious   buf_1          
# base
  5  0  0  0  0  0    13.92
