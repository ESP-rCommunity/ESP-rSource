# geometry of manager_a defined in: ../zones/manager_a.geo
GEN  manager_a  manager describes cellular office on left  # type, name, descr
      40      22   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      3.00000     0.00000     0.00000  # vert   2
      3.00000     4.50000     0.00000  # vert   3
      1.20000     4.50000     0.00000  # vert   4
      0.00000     4.50000     0.00000  # vert   5
      0.00000     0.00000     3.00000  # vert   6
      3.00000     0.00000     3.00000  # vert   7
      3.00000     4.50000     3.00000  # vert   8
      1.20000     4.50000     3.00000  # vert   9
      0.00000     4.50000     3.00000  # vert  10
      0.00000     0.00000     0.90000  # vert  11
      3.00000     0.00000     0.90000  # vert  12
      1.00000     0.00000     0.90000  # vert  13
      2.00000     0.00000     0.90000  # vert  14
      0.10000     0.00000     1.00000  # vert  15
      2.90000     0.00000     1.00000  # vert  16
      2.90000     0.00000     2.90000  # vert  17
      0.10000     0.00000     2.90000  # vert  18
      2.90000     4.50000     0.10000  # vert  19
      1.30000     4.50000     0.10000  # vert  20
      1.30000     4.50000     2.90000  # vert  21
      2.90000     4.50000     2.90000  # vert  22
      0.40000     4.50000     0.00000  # vert  23
      0.40000     4.50000     3.00000  # vert  24
      0.00000     2.50000     0.00000  # vert  25
      0.00000     2.35000     0.00000  # vert  26
      0.00000     2.50000     3.00000  # vert  27
      0.00000     2.35000     3.00000  # vert  28
      0.40000     4.50000     2.90000  # vert  29
      1.20000     4.50000     2.90000  # vert  30
      0.00000     1.00000     0.00000  # vert  31
      0.00000     1.00000     3.00000  # vert  32
      3.00000     1.00000     0.00000  # vert  33
      3.00000     2.35000     0.00000  # vert  34
      3.00000     1.00000     3.00000  # vert  35
      3.00000     2.35000     3.00000  # vert  36
      0.10000     0.10000     0.90000  # vert  37
      2.90000     0.10000     0.90000  # vert  38
      2.90000     1.10000     0.90000  # vert  39
      0.10000     1.10000     0.90000  # vert  40
# no of vertices followed by list of associated vert
   5,  2, 33, 35,  7, 12,
  11,  3,  4, 30,  9,  8,  3, 19, 22, 21, 20, 19,
   4,  4, 23, 29, 30,
   4,  5, 25, 27, 10,
   4,  6,  7, 35, 32,
   4,  1, 31, 33,  2,
   6,  1,  2, 12, 14, 13, 11,
  12, 11, 13, 14, 12,  7,  6, 11, 15, 18, 17, 16, 15,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
   7, 23,  5, 10, 24,  9, 30, 29,
   4, 25, 26, 28, 27,
   4, 26, 31, 32, 28,
   4, 33, 34, 36, 35,
   4, 34,  3,  8, 36,
   5, 31,  1, 11,  6, 32,
   4, 33, 31, 26, 34,
   7, 34, 26, 25,  5, 23,  4,  3,
   4, 32, 35, 36, 28,
   7, 28, 36,  8,  9, 24, 10, 27,
   4, 37, 38, 39, 40,
   4, 38, 37, 40, 39,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, pt_general    OPAQ  VERT  gyp_gyp_ptn  manager_b      
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
 14, pt_gen_a      OPAQ  VERT  gyp_gyp_ptn  manager_b      
 15, pt_gen_b      OPAQ  VERT  gyp_gyp_ptn  manager_b      
 16, pt_other_c    OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
 17, floor_b       OPAQ  FLOR  susp_flr_re  SIMILAR        
 18, floor_c       OPAQ  FLOR  susp_flr_re  SIMILAR        
 19, ceiling_b     OPAQ  CEIL  ceiling      SIMILAR        
 20, ceiling_c     OPAQ  CEIL  ceiling      SIMILAR        
 21, desk          OPAQ  CEIL  door         ADIABATIC      
 22, desk_a        OPAQ  FLOR  door         ADIABATIC      
# base
  6 17 18 22  0  0    16.30 0
