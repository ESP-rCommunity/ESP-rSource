# geometry of reception defined in: ../zones/reception.geo
GEN  reception  reception of office group adj to conference room  # type, name, descr
      47      18   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.00000     6.00000     0.00000  # vert   1
      3.00000     6.00000     3.00000  # vert   2
      9.00000     6.00000     0.00000  # vert   3
      9.00000     6.00000     3.00000  # vert   4
     12.00000     6.00000     0.00000  # vert   5
     12.00000     6.00000     3.00000  # vert   6
     12.00000     6.00000     0.90000  # vert   7
      3.00000    13.50000     0.00000  # vert   8
      6.00000    13.50000     0.00000  # vert   9
      6.00000    13.50000     3.00000  # vert  10
      3.00000    13.50000     3.00000  # vert  11
      9.00000    12.00000     0.00000  # vert  12
      9.00000    12.00000     3.00000  # vert  13
     12.00000    12.00000     0.00000  # vert  14
     12.00000    12.00000     0.90000  # vert  15
     12.00000    12.00000     3.00000  # vert  16
     12.00000     9.00000     0.00000  # vert  17
     12.00000     9.00000     0.90000  # vert  18
     12.00000     8.00000     0.90000  # vert  19
     12.00000     8.00000     0.50000  # vert  20
     12.00000     7.00000     0.50000  # vert  21
     12.00000     7.00000     0.90000  # vert  22
     12.00000     9.00000     3.00000  # vert  23
     12.00000     6.10000     1.00000  # vert  24
     12.00000     6.10000     2.90000  # vert  25
     12.00000     8.90000     2.90000  # vert  26
     12.00000     8.90000     1.00000  # vert  27
     12.00000    11.00000     0.90000  # vert  28
     12.00000    11.00000     0.50000  # vert  29
     12.00000    10.00000     0.50000  # vert  30
     12.00000    10.00000     0.90000  # vert  31
     12.00000     9.10000     1.00000  # vert  32
     12.00000     9.10000     2.90000  # vert  33
     12.00000    11.90000     2.90000  # vert  34
     12.00000    11.90000     1.00000  # vert  35
      9.00000     6.10000     0.90000  # vert  36
     11.90000     6.10000     0.90000  # vert  37
     11.90000    11.90000     0.90000  # vert  38
      9.00000    11.90000     0.90000  # vert  39
      9.00000    11.10000     0.90000  # vert  40
     11.10000    11.10000     0.90000  # vert  41
     11.10000     9.10000     0.90000  # vert  42
      9.00000     9.10000     0.90000  # vert  43
      9.00000     8.10000     0.90000  # vert  44
     11.10000     8.10000     0.90000  # vert  45
     11.10000     6.90000     0.90000  # vert  46
      9.00000     6.90000     0.90000  # vert  47
# no of vertices followed by list of associated vert
   4,  8,  1,  2, 11,
   4,  1,  3,  4,  2,
   5,  3,  5,  7,  6,  4,
   4,  9,  8, 11, 10,
   4, 12,  9, 10, 13,
   5, 14, 12, 13, 16, 15,
   8,  5, 17, 18, 19, 20, 21, 22,  7,
   4, 21, 20, 19, 22,
  12,  7, 22, 19, 18, 23,  6,  7, 24, 25, 26, 27, 24,
   4, 24, 27, 26, 25,
   8, 17, 14, 15, 28, 29, 30, 31, 18,
   4, 30, 29, 28, 31,
  12, 18, 31, 28, 15, 16, 23, 18, 32, 33, 34, 35, 32,
   4, 32, 35, 34, 33,
  12, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
  12, 37, 36, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38,
   8,  2,  4,  6, 23, 16, 13, 10, 11,
   8,  5,  3,  1,  8,  9, 12, 14, 17,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, pt_core_a     OPAQ  VERT  gyp_blk_ptn  SIMILAR        
  2, fict_rec      TRAN  VERT  fict         general        
  3, pt_rec_a      OPAQ  VERT  gyp_gyp_ptn  general        
  4, pt_recp_a     OPAQ  VERT  gyp_gyp_ptn  conference     
  5, fict_open     TRAN  VERT  fict         conference     
  6, pt_recp_b     OPAQ  VERT  gyp_gyp_ptn  conference     
  7, spandral      OPAQ  VERT  extern_wall  EXTERIOR       
  8, vent          OPAQ  VERT  insul_frame  EXTERIOR       
  9, frame         OPAQ  VERT  insul_frame  EXTERIOR       
 10, glazing       TRAN  VERT  dbl_glz      EXTERIOR       
 11, spandral_a    OPAQ  VERT  extern_wall  EXTERIOR       
 12, vent_a        OPAQ  VERT  insul_frame  EXTERIOR       
 13, frame_a       OPAQ  VERT  insul_frame  EXTERIOR       
 14, glazing_a     TRAN  VERT  dbl_glz      EXTERIOR       
 15, desk_top      OPAQ  CEIL  door         ADIABATIC      
 16, desk_base     OPAQ  FLOR  door         ADIABATIC      
 17, ceiling       OPAQ  CEIL  ceiling      ceil_void      
 18, floor         OPAQ  FLOR  susp_flr_re  SIMILAR        
# base
 16  0  0  0  0  0    10.10
