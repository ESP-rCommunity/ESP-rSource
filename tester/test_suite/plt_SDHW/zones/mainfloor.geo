# geometry of mainfloor defined in: ../zones/mainfloor.geo
GEN  mainfloor  mainfloor describes the main floor of the house as 1 zone, not i  # type, name, descr
      50      19   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     2.60000  # vert   1
     12.00000     0.00000     2.60000  # vert   2
     12.00000     6.79000     2.60000  # vert   3
      4.97000     6.79000     2.60000  # vert   4
      4.97000    12.20000     2.60000  # vert   5
      0.00000    12.20000     2.60000  # vert   6
      0.00000     7.00000     2.60000  # vert   7
      0.00000     0.00000     5.62000  # vert   8
     12.00000     0.00000     5.62000  # vert   9
     12.00000     6.79000     5.62000  # vert  10
      4.97000     6.79000     5.62000  # vert  11
      4.97000    12.20000     5.62000  # vert  12
      0.00000    12.20000     5.62000  # vert  13
      0.00000     7.00000     5.62000  # vert  14
      0.00000     2.70000     3.35000  # vert  15
      0.00000     1.07000     3.35000  # vert  16
      0.00000     1.07000     5.19000  # vert  17
      0.00000     2.70000     5.19000  # vert  18
      0.00000     5.93000     2.60000  # vert  19
      0.00000     4.13000     2.60000  # vert  20
      0.00000    11.20000     3.20000  # vert  21
      0.00000    10.17000     3.20000  # vert  22
      0.00000    10.17000     5.03000  # vert  23
      0.00000    11.20000     5.03000  # vert  24
      0.00000     8.67000     3.20000  # vert  25
      0.00000     7.64000     3.20000  # vert  26
      0.00000     7.64000     5.03000  # vert  27
      0.00000     8.67000     5.03000  # vert  28
     12.00000     4.25000     2.60000  # vert  29
     12.00000     5.35000     2.60000  # vert  30
     12.00000     1.00000     3.20000  # vert  31
     12.00000     3.43000     3.20000  # vert  32
     12.00000     3.43000     5.04000  # vert  33
     12.00000     1.00000     5.04000  # vert  34
      3.97000    12.20000     3.20000  # vert  35
      3.34000    12.20000     3.20000  # vert  36
      3.34000    12.20000     5.04000  # vert  37
      3.97000    12.20000     5.04000  # vert  38
      1.63000    12.20000     3.20000  # vert  39
      1.00000    12.20000     3.20000  # vert  40
      1.00000    12.20000     5.04000  # vert  41
      1.63000    12.20000     5.04000  # vert  42
      4.83000     2.27000     2.60000  # vert  43
      4.83000     0.00000     2.60000  # vert  44
      4.83000     0.00000     5.62000  # vert  45
      4.83000     2.27000     5.62000  # vert  46
      7.25000     2.27000     2.60000  # vert  47
      7.25000     2.27000     5.62000  # vert  48
      7.25000     0.00000     5.62000  # vert  49
      7.25000     0.00000     2.60000  # vert  50
# no of vertices followed by list of associated vert
   4,  1, 44, 45,  8,
  12,  2, 29, 30,  3, 10,  9,  2, 31, 34, 33, 32, 31,
   4,  3,  4, 11, 10,
   4,  4,  5, 12, 11,
  16,  5,  6, 40, 39, 42, 41, 40,  6, 13, 12,  5, 35, 38, 37, 36, 35,
  16,  6,  7, 26, 25, 28, 27, 26,  7, 14, 13,  6, 21, 24, 23, 22, 21,
  12,  1, 16, 15, 18, 17, 16,  1,  8, 14,  7, 19, 20,
  11,  8, 45, 46, 48, 49,  9, 10, 11, 12, 13, 14,
   4, 15, 16, 17, 18,
   4, 21, 22, 23, 24,
   4, 25, 26, 27, 28,
   4, 31, 32, 33, 34,
   4, 35, 36, 37, 38,
   4, 39, 40, 41, 42,
   4, 44, 43, 46, 45,
   4, 43, 47, 48, 46,
   4, 49, 48, 47, 50,
   4,  9, 49, 50,  2,
  15, 44,  1, 20, 19,  7,  6,  5,  4,  3, 30, 29,  2, 50, 47, 43,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, sidewall_1    OPAQ  VERT  ccht_wall    EXTERIOR       
  2, front         OPAQ  VERT  ccht_wall    EXTERIOR       
  3, garage_share  OPAQ  VERT  ccht_wall    garage         
  4, garage_back   OPAQ  VERT  ccht_wall    garage         
  5, sidewall_2    OPAQ  VERT  ccht_wall    EXTERIOR       
  6, back_2        OPAQ  VERT  ccht_wall    EXTERIOR       
  7, back_1        OPAQ  VERT  ccht_wall    EXTERIOR       
  8, main_ceiling  OPAQ  CEIL  floors_r     secondfloor    
  9, w_dining      TRAN  VERT  ccht_window  EXTERIOR       
 10, w_family1     TRAN  VERT  ccht_window  EXTERIOR       
 11, w_family2     TRAN  VERT  ccht_window  EXTERIOR       
 12, w_living      TRAN  VERT  ccht_window  EXTERIOR       
 13, w_side1       TRAN  VERT  ccht_window  EXTERIOR       
 14, w_side2       TRAN  VERT  ccht_window  EXTERIOR       
 15, stair_north   OPAQ  VERT  int_partitn  stairwell      
 16, stair_east    TRAN  VERT  fictitious   stairwell      
 17, stair_south   OPAQ  VERT  int_partitn  stairwell      
 18, sidewallfron  OPAQ  VERT  ccht_wall    EXTERIOR       
 19, main_floor    OPAQ  FLOR  floors       basement       
# base
  19   0   0   0   0   0   102.87
