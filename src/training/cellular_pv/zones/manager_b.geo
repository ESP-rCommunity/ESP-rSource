# geometry of manager_b defined in: ../zones/manager_b.geo
GEN  manager_b  manager_b describes right office  # type, name, descr
      28      13   0.000    # vertices, surfaces, rotation angle
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
      6.10000     0.00000     1.20900  # vert  11
      3.10000     0.00000     1.20900  # vert  12
      6.00000     0.00000     1.30900  # vert  13
      3.20000     0.00000     1.30900  # vert  14
      3.20000     0.00000     2.90000  # vert  15
      6.00000     0.00000     2.90000  # vert  16
      3.20000     4.50000     0.10000  # vert  17
      4.80000     4.50000     0.10000  # vert  18
      4.80000     4.50000     2.90000  # vert  19
      3.20000     4.50000     2.90000  # vert  20
      5.70000     4.50000     0.00000  # vert  21
      5.70000     4.50000     3.00000  # vert  22
      6.10000     2.50000     0.00000  # vert  23
      6.10000     2.35000     0.00000  # vert  24
      6.10000     2.50000     3.00000  # vert  25
      6.10000     2.35000     3.00000  # vert  26
      4.90000     4.50000     2.90000  # vert  27
      5.70000     4.50000     2.90000  # vert  28
# no of vertices followed by list of associated vert
   5,  3,  2, 12,  7,  8,
  11,  4,  3, 17, 18, 19, 20, 17,  3,  8,  9, 27,
   4, 21,  4, 27, 28,
   4, 23,  5, 10, 25,
   8,  7,  6, 26, 25, 10, 22,  9,  8,
   8, 24,  1,  2,  3,  4, 21,  5, 23,
   4,  2,  1, 11, 12,
  10, 11, 13, 14, 15, 16, 13, 11,  6,  7, 12,
   4, 14, 13, 16, 15,
   4, 18, 17, 20, 19,
   7,  5, 21, 28, 27,  9, 22, 10,
   4, 24, 23, 25, 26,
   5,  1, 24, 26,  6, 11,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, pt_general    OPAQ  VERT  gyp_gyp_ptn  manager_a      
  2, part_frame    OPAQ  VERT  insul_frame  corridor       
  3, door          OPAQ  VERT  door         corridor       
  4, pt_other      OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
  5, ceiling       OPAQ  CEIL  ceiling      SIMILAR        
  6, floor         OPAQ  FLOR  susp_flr_re  SIMILAR        
  7, spandrel      OPAQ  VERT  insul_frame  EXTERIOR       
  8, frame         OPAQ  VERT  insul_frame  EXTERIOR       
  9, glazing       TRAN  VERT  dbl_glz      EXTERIOR       
 10, part_glaz     TRAN  VERT  dbl_glz      corridor       
 11, ptn_corid     OPAQ  VERT  gyp_gyp_ptn  corridor       
 12, pt_other_a    OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
 13, pt_other_b    OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
# base
  6  0  0  0  0  0    13.50 0
