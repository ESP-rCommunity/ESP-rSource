# geometry of zone_L defined in: ../zones/zone_L.geo
GEN  zone_L  zone_L describes a...  # type, name, descr
      23      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.00000     3.54000     0.00000  # vert   1
      5.50000     3.54000     0.00000  # vert   2
      5.50000     7.50000     0.00000  # vert   3
      0.00000     7.50000     0.00000  # vert   4
      0.00000     5.00000     0.00000  # vert   5
      3.00000     5.00000     0.00000  # vert   6
      3.00000     3.54000     2.80000  # vert   7
      5.50000     3.54000     2.80000  # vert   8
      5.50000     7.50000     2.80000  # vert   9
      0.00000     7.50000     2.80000  # vert  10
      0.00000     5.00000     2.80000  # vert  11
      3.00000     5.00000     2.80000  # vert  12
      3.00000     7.50000     2.80000  # vert  13
      4.00000     7.50000     0.80000  # vert  14
      1.50000     7.50000     0.80000  # vert  15
      1.50000     7.50000     2.00000  # vert  16
      4.00000     7.50000     2.00000  # vert  17
      2.00000     5.00000     0.00000  # vert  18
      1.00000     5.00000     0.00000  # vert  19
      1.00000     5.00000     2.80000  # vert  20
      2.00000     5.00000     2.80000  # vert  21
      1.00000     5.00000     1.00000  # vert  22
      2.00000     5.00000     1.00000  # vert  23
# no of vertices followed by list of associated vert
   4,  1,  2,  8,  7,
   4,  2,  3,  9,  8,
  11,  3,  4, 10, 13,  9,  3, 14, 17, 16, 15, 14,
   4,  4,  5, 11, 10,
   4,  6,  1,  7, 12,
   5,  7,  8,  9, 13, 12,
   8,  1,  6, 18, 19,  5,  4,  3,  2,
   6, 12, 13, 10, 11, 20, 21,
   4, 14, 15, 16, 17,
   5, 18,  6, 12, 21, 23,
   5,  5, 19, 22, 20, 11,
   4, 22, 19, 18, 23,
   4, 22, 23, 21, 20,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  int_part     zone_S         
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, part-2a       OPAQ  VERT  int_part     room           
  6, ceil_2        OPAQ  CEIL  ceiling      roof_2         
  7, floor         OPAQ  FLOR  floor_1      GROUND         
  8, ceil_1        OPAQ  CEIL  ceiling      roof_1         
  9, window        TRAN  VERT  d_glz        EXTERIOR       
 10, part-2b       OPAQ  VERT  int_part     room           
 11, part-2d       OPAQ  VERT  int_part     room           
 12, radiator      OPAQ  VERT  int_part     room           
 13, part-2c       OPAQ  VERT  int_part     room           
# base
  7  0  0  0  0  0    17.40 0
