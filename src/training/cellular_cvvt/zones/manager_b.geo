# geometry of manager_b defined in: ../zones/manager_b.geo
GEN  manager_b  manager_b describes right office  # type, name, descr
      30      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.10000     0.00000     0.00000  # vert   1
      3.10000     0.00000     0.00000  # vert   2
      3.10000     4.50000     0.00000  # vert   3
      4.90000     4.50000     0.00000  # vert   4
      6.10000     4.50000     0.00000  # vert   5
      6.10000     0.00000     3.00000  # vert   6
      3.10000     0.00000     3.00000  # vert   7
      3.10000     4.50000     3.00000  # vert   8
      4.90000     4.50000     3.00000  # vert   9
      6.10000     4.50000     3.00000  # vert  10
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
      6.10000     2.50000     0.00000  # vert  25
      6.10000     2.35000     0.00000  # vert  26
      6.10000     2.50000     3.00000  # vert  27
      6.10000     2.35000     3.00000  # vert  28
      4.90000     4.50000     2.90000  # vert  29
      5.70000     4.50000     2.90000  # vert  30
# no of vertices followed by list of associated vert
   5,  3,  2, 12,  7,  8,
  11,  4,  3, 19, 20, 21, 22, 19,  3,  8,  9, 29,
   4, 23,  4, 29, 30,
   4, 25,  5, 10, 27,
   8,  7,  6, 28, 27, 10, 24,  9,  8,
   8, 26,  1,  2,  3,  4, 23,  5, 25,
   6,  2,  1, 11, 13, 14, 12,
  12, 13, 11, 15, 16, 17, 18, 15, 11,  6,  7, 12, 14,
   4, 16, 15, 18, 17,
   4, 20, 19, 22, 21,
   7,  5, 23, 30, 29,  9, 24, 10,
   4, 26, 25, 27, 28,
   5,  1, 26, 28,  6, 11,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
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
# base
  6  0  0  0  0  0    13.50 0
