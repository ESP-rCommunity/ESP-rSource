# geometry of west_space defined in: ../zone/west_space.geo
GEN  west_space  west_space is the construction void within west wall  # type, name, descr
      24      14   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.28600     5.58600     0.00000  # vert   1
      8.48600     5.93200     0.00000  # vert   2
      3.11600     9.03200     0.00000  # vert   3
      2.91600     8.68600     0.00000  # vert   4
      8.28600     5.58600     2.40000  # vert   5
      8.48600     5.93200     2.40000  # vert   6
      3.11600     9.03200     2.40000  # vert   7
      2.91600     8.68600     2.40000  # vert   8
      3.52300     8.33600     0.60000  # vert   9
      4.25900     7.91100     0.60000  # vert  10
      4.25900     7.91100     2.10000  # vert  11
      3.52300     8.33600     2.10000  # vert  12
      6.81300     6.43600     0.60000  # vert  13
      7.55000     6.01100     0.60000  # vert  14
      7.55000     6.01100     2.10000  # vert  15
      6.81300     6.43600     2.10000  # vert  16
      7.79300     6.33300     0.60000  # vert  17
      7.05600     6.75800     0.60000  # vert  18
      7.05600     6.75800     2.10000  # vert  19
      7.79300     6.33300     2.10000  # vert  20
      4.50200     8.23300     0.60000  # vert  21
      3.76600     8.65800     0.60000  # vert  22
      3.76600     8.65800     2.10000  # vert  23
      4.50200     8.23300     2.10000  # vert  24
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
  16,  2,  3, 22, 21, 24, 23, 22,  3,  7,  6,  2, 17, 20, 19, 18, 17,
   4,  3,  4,  8,  7,
  16,  4,  1, 14, 13, 16, 15, 14,  1,  5,  8,  4,  9, 12, 11, 10,  9,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 22, 23, 12,
   4, 21, 10, 11, 24,
   4,  9, 10, 21, 22,
   4, 11, 12, 23, 24,
   4, 13, 18, 19, 16,
   4, 17, 14, 15, 20,
   4, 13, 14, 17, 18,
   4, 15, 16, 19, 20,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    2  10   9   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, s_buf         OPAQ  VERT  int_buf      buf_1          
  2, s_liv         OPAQ  VERT  int_westrev  kitliv         
  3, s_adjhouse    OPAQ  VERT  ext_west     SIMILAR        
  4, ext_west      OPAQ  VERT  ext_west     EXTERIOR       
  5, ceil          OPAQ  CEIL  int_roof     roof           
  6, floor         OPAQ  FLOR  house_floor  GROUND         
  7, jambl         OPAQ  VERT  ext_west     EXTERIOR       
  8, jambr         OPAQ  VERT  ext_west     EXTERIOR       
  9, sill          OPAQ  CEIL  ext_west     EXTERIOR       
 10, head          OPAQ  FLOR  ext_west     EXTERIOR       
 11, jl            OPAQ  VERT  ext_west     EXTERIOR       
 12, jr            OPAQ  VERT  ext_west     EXTERIOR       
 13, sillr         OPAQ  CEIL  ext_west     EXTERIOR       
 14, headr         OPAQ  FLOR  ext_west     EXTERIOR       
# base
  6  0  0  0  0  0     2.48 2
