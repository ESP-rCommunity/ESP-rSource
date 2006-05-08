# geometry of kitliv defined in: ../zone/kitliv.geo
GEN  kitliv  kitliv describes a...  # type, name, descr
      22      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.48800     5.93200     0.00000  # vert   1
     10.58800     9.56900     0.00000  # vert   2
      5.21900    12.66900     0.00000  # vert   3
      3.11900     9.03200     0.00000  # vert   4
      8.48800     5.93200     2.40000  # vert   5
     10.58800     9.56900     2.40000  # vert   6
      5.21900    12.66900     2.40000  # vert   7
      3.11900     9.03200     2.40000  # vert   8
      3.76800     8.65700     0.60000  # vert   9
      4.50400     8.23200     0.60000  # vert  10
      4.50400     8.23200     2.10000  # vert  11
      3.76800     8.65700     2.10000  # vert  12
      7.05900     6.75700     0.60000  # vert  13
      7.79500     6.33200     0.60000  # vert  14
      7.79500     6.33200     2.10000  # vert  15
      7.05900     6.75700     2.10000  # vert  16
      9.28800     7.31700     0.60000  # vert  17
     10.08800     8.70400     0.60000  # vert  18
     10.08800     8.70400     2.10000  # vert  19
      9.28800     7.31700     2.10000  # vert  20
      7.12300    11.56900     0.00000  # vert  21
      7.12300    11.56900     2.40000  # vert  22
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1, 17, 20, 19, 18, 17,
   4,  2, 21, 22,  6,
   4,  3,  4,  8,  7,
  16,  4,  1, 14, 13, 16, 15, 14,  1,  5,  8,  4,  9, 12, 11, 10,  9,
   5,  5,  6, 22,  7,  8,
   5,  1,  4,  3, 21,  2,
   4,  9, 10, 11, 12,
   4, 17, 18, 19, 20,
   4, 13, 14, 15, 16,
   4, 21,  3,  7, 22,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, s_buf         OPAQ  VERT  int_buf      buf_1          
  2, s_hall        OPAQ  VERT  int_buf      hall           
  3, s_adjhouse    OPAQ  VERT  ext_west     SIMILAR        
  4, s_west        OPAQ  VERT  int_west     west_space     
  5, ceil          OPAQ  CEIL  int_roof     roof           
  6, floor         OPAQ  FLOR  house_floor  GROUND         
  7, win1          TRAN  VERT  doub_glaz    EXTERIOR       
  8, win3          TRAN  VERT  doub_glaz    buf_1          
  9, win2          TRAN  VERT  doub_glaz    EXTERIOR       
 10, s_bath        OPAQ  VERT  int_buf      bath           
# base
  6  0  0  0  0  0    26.04
