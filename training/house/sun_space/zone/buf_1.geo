# geometry of buf_1 defined in: ../zone/buf_1.geo
GEN  buf_1  buf_1 describes a...  # type, name, descr
      18       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.79800     4.13600     0.00000  # vert   1
      8.28600     5.58600     0.00000  # vert   2
     10.79800     4.13600     2.40000  # vert   3
      8.28600     5.58600     2.40000  # vert   4
      8.46000     5.48500     0.30000  # vert   5
     10.53800     4.28500     0.30000  # vert   6
     10.53800     4.28500     2.10000  # vert   7
      8.46000     5.48500     2.10000  # vert   8
      8.48600     5.93100     0.00000  # vert   9
     10.58600     9.56900     0.00000  # vert  10
     10.58600     9.56900     2.40000  # vert  11
      8.48600     5.93100     2.40000  # vert  12
     10.08600     8.70300     0.60000  # vert  13
      9.28600     7.31800     0.60000  # vert  14
      9.28600     7.31800     2.10000  # vert  15
     10.08600     8.70300     2.10000  # vert  16
     13.09800     8.12000     0.00000  # vert  17
     13.09800     8.12000     2.40000  # vert  18
# no of vertices followed by list of associated vert
   4,  1, 17, 18,  3,
  10,  2,  1,  3,  4,  2,  5,  8,  7,  6,  5,
   5,  3, 18, 11, 12,  4,
   5,  1,  2,  9, 10, 17,
   4,  5,  6,  7,  8,
  10, 10,  9, 12, 11, 10, 13, 16, 15, 14, 13,
   4,  9,  2,  4, 12,
   4, 13, 14, 15, 16,
   4, 17, 10, 11, 18,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, ext_wall      OPAQ  VERT  ext_buf      EXTERIOR       
  2, west          OPAQ  VERT  ext_west     EXTERIOR       
  3, fict          TRAN  CEIL  fictitious   buf_roof       
  4, floor         OPAQ  FLOR  buf_floor    GROUND         
  5, westwin       TRAN  VERT  doub_glaz    EXTERIOR       
  6, s_liv         OPAQ  VERT  int_buf      kitliv         
  7, s_west        OPAQ  VERT  int_buf      west_space     
  8, int_win       TRAN  VERT  doub_glaz    kitliv         
  9, fictv         TRAN  VERT  fictitious   buf_2          
# base
  4  0  0  0  0  0    13.34
