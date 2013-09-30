# geometry of secondfloor defined in: ../zones/secondfloor.geo
GEN  secondfloor  secondfloor describes the second floor of the ccht house as 1 zo  # type, name, descr
      43      17   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     5.62000  # vert   1
     12.00000     0.00000     5.62000  # vert   2
     12.00000    12.20000     5.62000  # vert   3
      0.00000    12.20000     5.62000  # vert   4
      0.00000     0.00000     8.08500  # vert   5
     12.00000     0.00000     8.08500  # vert   6
     12.00000    12.20000     8.08500  # vert   7
      0.00000    12.20000     8.08500  # vert   8
     12.00000     1.00000     6.02000  # vert   9
     12.00000     3.43000     6.02000  # vert  10
     12.00000     3.43000     7.66000  # vert  11
     12.00000     1.00000     7.66000  # vert  12
     12.00000     4.25000     6.62000  # vert  13
     12.00000     5.88000     6.62000  # vert  14
     12.00000     5.88000     7.66000  # vert  15
     12.00000     4.25000     7.66000  # vert  16
     12.00000     7.00000     6.41000  # vert  17
     12.00000     9.22000     6.41000  # vert  18
     12.00000     9.22000     7.66000  # vert  19
     12.00000     7.00000     7.66000  # vert  20
      0.00000     2.83000     6.02000  # vert  21
      0.00000     1.00000     6.02000  # vert  22
      0.00000     1.00000     7.26000  # vert  23
      0.00000     2.83000     7.26000  # vert  24
      0.00000     8.20000     6.02000  # vert  25
      0.00000     6.97000     6.02000  # vert  26
      0.00000     6.97000     7.26000  # vert  27
      0.00000     8.20000     7.26000  # vert  28
      0.00000     4.45000     6.22000  # vert  29
      0.00000     3.82000     6.22000  # vert  30
      0.00000     3.82000     7.26000  # vert  31
      0.00000     4.45000     7.26000  # vert  32
      4.83000     2.27000     8.06500  # vert  33
      4.83000     0.00000     8.06500  # vert  34
      4.83000     0.00000     5.62000  # vert  35
      4.83000     2.27000     5.62000  # vert  36
      7.25000     2.27000     5.62000  # vert  37
      7.25000     2.27000     8.06500  # vert  38
      7.25000     0.00000     8.06500  # vert  39
      7.25000     0.00000     5.62000  # vert  40
      4.97000    12.20000     5.62000  # vert  41
      4.97000     6.79000     5.62000  # vert  42
     12.00000     6.79000     5.62000  # vert  43
# no of vertices followed by list of associated vert
   4,  1, 35, 34,  5,
  23,  6, 12, 11, 10,  9, 12,  6,  2, 43, 14, 13, 16, 15, 14, 43,  3, 18, 17, 20, 19, 18,  3,  7,
   5,  3, 41,  4,  8,  7,
  22,  4,  1, 22, 23, 24, 21, 22,  1,  5, 31, 32, 29, 30, 31,  5,  8,  4, 25, 26, 27, 28, 25,
   8,  5, 34, 33, 38, 39,  6,  7,  8,
  10,  1,  4, 41, 42, 43,  2, 40, 37, 36, 35,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
   4, 22, 21, 24, 23,
   4, 26, 25, 28, 27,
   4, 30, 29, 32, 31,
   4, 33, 34, 35, 36,
   4, 37, 38, 33, 36,
   4, 39, 38, 37, 40,
   4, 39, 40,  2,  6,
   4, 42, 41,  3, 43,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, 2nd_rt_side   OPAQ  VERT  ccht_wall    EXTERIOR       
  2, 2nd_front     OPAQ  VERT  ccht_wall    EXTERIOR       
  3, 2ns_l_side    OPAQ  VERT  ccht_wall    EXTERIOR       
  4, 2nd_back      OPAQ  VERT  ccht_wall    EXTERIOR       
  5, 2nd_ceiling   OPAQ  CEIL  ceiling      roof           
  6, 2nd_floor     OPAQ  FLOR  floors       mainfloor      
  7, w_bedrm2      TRAN  VERT  ccht_window  EXTERIOR       
  8, w_bathroom    TRAN  VERT  ccht_window  EXTERIOR       
  9, w_master      TRAN  VERT  ccht_window  EXTERIOR       
 10, w_bdroom3     TRAN  VERT  ccht_window  EXTERIOR       
 11, w_bedroom4    TRAN  VERT  ccht_window  EXTERIOR       
 12, w_bathrom2    TRAN  VERT  ccht_window  EXTERIOR       
 13, stair_north   OPAQ  VERT  int_partitn  stairwell      
 14, stair_east    TRAN  VERT  fictitious   stairwell      
 15, stair_south   OPAQ  VERT  int_partitn  stairwell      
 16, 2nd_rt_side2  OPAQ  VERT  ccht_wall    EXTERIOR       
 17, flr_abv_gar   OPAQ  FLOR  exp_flr_r    garage         
# base
   6  17   0   0   0   0   140.91
