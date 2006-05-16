# geometry of main_second defined in: ../zones/main_second.geo
GEN  main_second  simplified elevation of main and second floors  # type, name, descr
      50      18   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     2.60000  # vert   1
     12.00000     0.00000     2.60000  # vert   2
      0.00000    12.20000     2.60000  # vert   3
      0.00000     0.00000     8.08500  # vert   4
     12.00000     0.00000     8.08500  # vert   5
     12.00000    12.20000     8.08500  # vert   6
      0.00000    12.20000     8.08500  # vert   7
      3.97000    12.20000     3.10000  # vert   8
      3.43000    12.20000     3.10000  # vert   9
      3.43000    12.20000     4.94000  # vert  10
      3.97000    12.20000     4.94000  # vert  11
      1.63000    12.20000     3.10000  # vert  12
      1.00000    12.20000     3.10000  # vert  13
      1.00000    12.20000     4.94000  # vert  14
      1.63000    12.20000     4.94000  # vert  15
     12.00000     1.00000     3.10000  # vert  16
     12.00000     3.43000     3.10000  # vert  17
     12.00000     3.43000     4.94000  # vert  18
     12.00000     1.00000     4.94000  # vert  19
     12.00000     1.00000     6.00000  # vert  20
     12.00000     3.43000     6.00000  # vert  21
     12.00000     3.43000     7.64000  # vert  22
     12.00000     1.00000     7.64000  # vert  23
     12.00000     8.64000     6.39000  # vert  24
     12.00000    10.86000     6.39000  # vert  25
     12.00000    10.86000     7.64000  # vert  26
     12.00000     8.64000     7.64000  # vert  27
      0.00000     2.76000     3.10000  # vert  28
      0.00000     1.13000     3.10000  # vert  29
      0.00000     1.13000     4.94000  # vert  30
      0.00000     2.76000     4.94000  # vert  31
      0.00000     2.76000     6.40000  # vert  32
      0.00000     0.93000     6.40000  # vert  33
      0.00000     0.93000     7.64000  # vert  34
      0.00000     2.76000     7.64000  # vert  35
      6.00000     0.00000     5.60000  # vert  36
      7.25000     0.00000     5.60000  # vert  37
      7.25000     0.00000     6.85000  # vert  38
      6.00000     0.00000     6.85000  # vert  39
      0.00000     8.00000     6.40000  # vert  40
      0.00000     6.14000     6.40000  # vert  41
      0.00000     6.14000     7.64000  # vert  42
      0.00000     8.00000     7.64000  # vert  43
      4.97000    12.20000     2.60000  # vert  44
      4.97000    12.20000     5.62000  # vert  45
     12.00000    12.20000     5.62000  # vert  46
     12.00000     6.79000     2.60000  # vert  47
      4.97000     6.79000     2.60000  # vert  48
      4.97000     6.79000     5.62000  # vert  49
     12.00000     6.79000     5.62000  # vert  50
# no of vertices followed by list of associated vert
  10,  1,  2,  5,  4, 39, 38, 37, 36, 39,  4,
  24, 46, 25, 24, 27, 26, 25, 46,  6,  5, 23, 22, 21, 20, 23,  5,  2, 16, 19, 18, 17, 16,  2, 47, 50,
  18,  7,  6, 46, 45, 44,  8, 11, 10,  9,  8, 44,  3, 13, 12, 15, 14, 13,  3,
  22,  3, 28, 31, 30, 29, 28,  3,  1, 33, 32, 35, 34, 33,  1,  4,  7, 40, 43, 42, 41, 40,  7,
   4,  4,  5,  6,  7,
   6,  1,  3, 44, 48, 47,  2,
   4,  8,  9, 10, 11,
   4, 12, 13, 14, 15,
   4, 16, 17, 18, 19,
   4, 20, 21, 22, 23,
   4, 24, 25, 26, 27,
   4, 28, 29, 30, 31,
   4, 32, 33, 34, 35,
   4, 36, 37, 38, 39,
   4, 40, 41, 42, 43,
   4, 47, 48, 49, 50,
   4, 44, 45, 49, 48,
   4, 45, 46, 50, 49,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  ccht_wall    EXTERIOR       
  2, Surf-2        OPAQ  VERT  ccht_wall    EXTERIOR       
  3, Surf-3        OPAQ  VERT  ccht_wall    EXTERIOR       
  4, Surf-4        OPAQ  VERT  ccht_wall    EXTERIOR       
  5, ceiling       OPAQ  CEIL  ceiling      roof           
  6, floor         OPAQ  FLOR  floors       basement       
  7, W5            TRAN  VERT  ccht_window  EXTERIOR       
  8, W6            TRAN  VERT  ccht_window  EXTERIOR       
  9, W1            TRAN  VERT  ccht_window  EXTERIOR       
 10, W2            TRAN  VERT  ccht_window  EXTERIOR       
 11, W4            TRAN  VERT  ccht_window  EXTERIOR       
 12, W9            TRAN  VERT  ccht_window  EXTERIOR       
 13, W12           TRAN  VERT  ccht_window  EXTERIOR       
 14, W13           TRAN  VERT  ccht_window  EXTERIOR       
 15, W10_11        TRAN  VERT  ccht_window  EXTERIOR       
 16, garage_in     OPAQ  VERT  ccht_wall    garage         
 17, garage_back   OPAQ  VERT  ccht_wall    garage         
 18, garage_ceil   OPAQ  FLOR  exp_flr      garage         
# base
   6   0   0   0   0   0   108.37
