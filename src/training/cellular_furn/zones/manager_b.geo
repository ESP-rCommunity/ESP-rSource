# geometry of manager_b defined in: ../zones/manager_b.geo
GEN  manager_b  manager_b describes right higher resolution office  # type, name, descr
      48      26   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.10000     0.00000     0.00000  # vert   1
      3.10000     0.00000     0.00000  # vert   2
      3.10000     4.50000     0.00000  # vert   3
      4.90000     4.50000     0.00000  # vert   4
      7.10000     4.50000     0.00000  # vert   5
      6.10000     0.00000     3.00000  # vert   6
      3.10000     0.00000     3.00000  # vert   7
      3.10000     4.50000     3.00000  # vert   8
      4.90000     4.50000     3.00000  # vert   9
      7.10000     4.50000     3.00000  # vert  10
      6.10000     0.00000     0.90000  # vert  11
      3.10000     0.00000     0.90000  # vert  12
      5.10000     0.00000     0.90000  # vert  13
      4.10000     0.00000     0.90000  # vert  14
      6.00000     0.00000     1.00000  # vert  15
      3.20000     0.00000     1.00000  # vert  16
      3.20000     0.00000     2.90000  # vert  17
      6.00000     0.00000     2.90000  # vert  18
      3.20000     4.50000     0.10000  # vert  19
      4.80000     4.50000     0.10000  # vert  20
      4.80000     4.50000     2.90000  # vert  21
      3.20000     4.50000     2.90000  # vert  22
      5.70000     4.50000     0.00000  # vert  23
      5.70000     4.50000     3.00000  # vert  24
      7.10000     2.50000     0.00000  # vert  25
      6.10000     2.35000     0.00000  # vert  26
      7.10000     2.50000     3.00000  # vert  27
      6.10000     2.35000     3.00000  # vert  28
      5.70000     4.50000     2.90000  # vert  29
      4.90000     4.50000     2.90000  # vert  30
      6.10000     1.00000     0.00000  # vert  31
      6.10000     1.00000     3.00000  # vert  32
      3.10000     1.00000     0.00000  # vert  33
      3.10000     2.35000     0.00000  # vert  34
      3.10000     1.00000     3.00000  # vert  35
      3.10000     2.35000     3.00000  # vert  36
      3.20000     0.10000     0.90000  # vert  37
      6.00000     0.10000     0.90000  # vert  38
      6.00000     1.10000     0.90000  # vert  39
      3.20000     1.10000     0.90000  # vert  40
      6.10000     2.70000     0.10000  # vert  41
      7.00000     2.70000     0.10000  # vert  42
      7.00000     2.70000     2.10000  # vert  43
      6.10000     2.70000     2.10000  # vert  44
      6.10000     3.70000     0.10000  # vert  45
      7.00000     3.70000     0.10000  # vert  46
      7.00000     3.70000     2.10000  # vert  47
      6.10000     3.70000     2.10000  # vert  48
# no of vertices followed by list of associated vert
   5, 33,  2, 12,  7, 35,
  11,  4,  3, 19, 20, 21, 22, 19,  3,  8,  9, 30,
   4, 23,  4, 30, 29,
   4, 25,  5, 10, 27,
   4,  7,  6, 32, 35,
   4, 31,  1,  2, 33,
   6,  2,  1, 11, 13, 14, 12,
  12, 13, 11, 15, 16, 17, 18, 15, 11,  6,  7, 12, 14,
   4, 16, 15, 18, 17,
   4, 20, 19, 22, 21,
   7,  5, 23, 29, 30,  9, 24, 10,
   4, 26, 25, 27, 28,
   4, 31, 26, 28, 32,
   4, 34, 33, 35, 36,
   4,  3, 34, 36,  8,
   5,  1, 31, 32,  6, 11,
   4, 31, 33, 34, 26,
   7, 26, 34,  3,  4, 23,  5, 25,
   4, 35, 32, 28, 36,
   7, 36, 28, 27, 10, 24,  9,  8,
   4, 37, 38, 39, 40,
   4, 38, 37, 40, 39,
   4, 41, 42, 43, 44,
   4, 42, 41, 44, 43,
   4, 45, 46, 47, 48,
   4, 46, 45, 48, 47,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, pt_general    OPAQ  VERT  gyp_gyp_ptn  manager_a      
  2, part_frame    OPAQ  VERT  insul_frame  coridor        
  3, door          OPAQ  VERT  door         coridor        
  4, pt_other      OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
  5, ceiling       OPAQ  CEIL  ceiling      SIMILAR        
  6, floor         OPAQ  FLOR  susp_flr_re  SIMILAR        
  7, spandral      OPAQ  VERT  insul_frame  EXTERIOR       
  8, frame         OPAQ  VERT  insul_frame  EXTERIOR       
  9, glazing       TRAN  VERT  dbl_glz      EXTERIOR       
 10, part_glaz     TRAN  VERT  dbl_glz      coridor        
 11, ptn_corid     OPAQ  VERT  gyp_gyp_ptn  coridor        
 12, pt_other_a    OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
 13, pt_other_b    OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
 14, pt_gen_a      OPAQ  VERT  gyp_gyp_ptn  manager_a      
 15, pt_gen_b      OPAQ  VERT  gyp_gyp_ptn  manager_a      
 16, pt_other_c    OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
 17, floor_b       OPAQ  FLOR  susp_flr_re  SIMILAR        
 18, floor_c       OPAQ  FLOR  susp_flr_re  SIMILAR        
 19, ceiling_b     OPAQ  CEIL  ceiling      SIMILAR        
 20, ceiling_c     OPAQ  CEIL  ceiling      SIMILAR        
 21, desk          OPAQ  CEIL  door         ADIABATIC      
 22, desk_a        OPAQ  FLOR  door         ADIABATIC      
 23, cabinet_a     OPAQ  VERT  door         ADIABATIC      
 24, cabinet_aa    OPAQ  VERT  door         ADIABATIC      
 25, cabinet_b     OPAQ  VERT  door         ADIABATIC      
 26, cabinet_bb    OPAQ  VERT  door         ADIABATIC      
# base
  6 17 18 22  0  0    18.37 0
