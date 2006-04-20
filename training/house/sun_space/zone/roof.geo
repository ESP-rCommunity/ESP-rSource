# geometry of roof defined in: ../zone/roof.geo
GEN  roof  roof describes a...  # type, name, descr
      16      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.28600     5.58600     2.40000  # vert   1
     12.98600    13.72700     2.40000  # vert   2
      7.61600    16.82700     2.40000  # vert   3
      2.91600     8.68600     2.40000  # vert   4
      5.36600    12.93000     5.20000  # vert   5
     10.58600     9.57000     2.40000  # vert   6
     11.43600    11.04200     2.40000  # vert   7
      7.12100    11.56900     2.40000  # vert   8
      7.97100    13.04100     2.40000  # vert   9
      8.75100    12.59200     2.40000  # vert  10
     10.30100    15.27700     2.40000  # vert  11
      5.21700    12.67000     2.40000  # vert  12
      6.06600    14.14200     2.40000  # vert  13
      8.48600     5.93200     2.40000  # vert  14
      3.11600     9.03200     2.40000  # vert  15
     10.73600     9.83000     5.20000  # vert  16
# no of vertices followed by list of associated vert
   6,  3, 13, 12, 15,  4,  5,
   5, 14, 15, 12,  8,  6,
   5,  6,  8,  9, 10,  7,
   4,  8, 12, 13,  9,
   4,  7, 10, 11,  2,
   5, 10,  9, 13,  3, 11,
   4,  1,  4, 15, 14,
   6,  1, 14,  6,  7,  2, 16,
   4,  4,  1, 16,  5,
   5,  2, 11,  3,  5, 16,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    2   6   1   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, roof_n        OPAQ  VERT  ext_west     EXTERIOR       
  2, liv           OPAQ  FLOR  int_roofrev  kitliv         
  3, hall          OPAQ  FLOR  int_roofrev  hall           
  4, bath          OPAQ  FLOR  int_roofrev  bath           
  5, bed1          OPAQ  FLOR  int_roofrev  bed1           
  6, bed2          OPAQ  FLOR  int_roofrev  bed2           
  7, west_sp       OPAQ  FLOR  int_roofrev  west_space     
  8, roof_buf      OPAQ  VERT  int_buf      buf_roof       
  9, roof_w        OPAQ  SLOP  ni_roof      EXTERIOR       
 10, roof_e        OPAQ  SLOP  ni_roof      EXTERIOR       
# base
  2  3  4  5  6  7    58.28 0
