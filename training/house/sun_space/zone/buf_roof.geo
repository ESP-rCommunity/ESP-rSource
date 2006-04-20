# geometry of buf_roof defined in: ../zone/buf_roof.geo
GEN  buf_roof  buf_roof describes a...  # type, name, descr
      18       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.79800     4.13600     2.40000  # vert   1
     15.49800    12.27600     2.40000  # vert   2
     13.24800     8.37900     5.20000  # vert   3
      8.28600     5.58600     2.40000  # vert   4
     10.58600     9.56900     2.40000  # vert   5
     11.43600    11.04100     2.40000  # vert   6
     12.98600    13.72600     2.40000  # vert   7
     10.15200     8.01800     4.13600  # vert   8
     11.92800     6.99200     4.13600  # vert   9
     12.27400     7.59300     4.53300  # vert  10
     10.50000     8.61900     4.53300  # vert  11
     13.57900     9.95300     4.24900  # vert  12
     11.84700    10.95200     4.24900  # vert  13
     11.50700    10.36400     4.67200  # vert  14
     13.23900     9.36500     4.67200  # vert  15
      8.48600     5.93100     2.40000  # vert  16
     13.09800     8.12000     2.40000  # vert  17
     10.73600     9.82900     5.20000  # vert  18
# no of vertices followed by list of associated vert
   5,  1,  4, 16,  5, 17,
  10,  4,  1,  3, 18,  4,  8, 11, 10,  9,  8,
  10,  2,  7, 18,  3,  2, 12, 15, 14, 13, 12,
   4,  1, 17,  2,  3,
   4,  8,  9, 10, 11,
   4, 12, 13, 14, 15,
   5, 17,  5,  6,  7,  2,
   6,  7,  6,  5, 16,  4, 18,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    2   6   1   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, buf_1         TRAN  FLOR  fictitious   buf_1          
  2, roof_e        OPAQ  SLOP  roof         EXTERIOR       
  3, roof_w        OPAQ  SLOP  roof         EXTERIOR       
  4, roof_s        OPAQ  VERT  ext_buf      EXTERIOR       
  5, win2          TRAN  SLOP  doub_glaz    EXTERIOR       
  6, glz_w         TRAN  SLOP  doub_glaz    EXTERIOR       
  7, buf_2         TRAN  FLOR  fictitious   buf_2          
  8, roof_buf      OPAQ  VERT  int_buf      roof           
# base
  1  7  0  0  0  0    27.26
