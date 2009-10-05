# geometry of manager_a defined in: ../zones/manager_a.geo
GEN  manager_a  manager describes cellular office on left  # type, name, descr
      30      14   0.000    # vertices, surfaces, rotation angle
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
      0.00000     0.00000     1.20900  # vert  11
      3.00000     0.00000     1.20900  # vert  12
      0.10000     0.00000     1.30900  # vert  13
      2.90000     0.00000     1.30900  # vert  14
      2.90000     0.00000     2.90000  # vert  15
      0.10000     0.00000     2.90000  # vert  16
      2.90000     4.50000     0.10000  # vert  17
      1.30000     4.50000     0.10000  # vert  18
      1.30000     4.50000     2.90000  # vert  19
      2.90000     4.50000     2.90000  # vert  20
      0.40000     4.50000     0.00000  # vert  21
      0.40000     4.50000     3.00000  # vert  22
      0.00000     2.50000     0.00000  # vert  23
      0.00000     2.35000     0.00000  # vert  24
      0.00000     2.50000     3.00000  # vert  25
      0.00000     2.35000     3.00000  # vert  26
      0.40000     4.50000     2.90000  # vert  27
      1.20000     4.50000     2.90000  # vert  28
      2.68500     0.00000     0.00000  # vert  29
      2.68500     0.00000     1.20900  # vert  30
# no of vertices followed by list of associated vert
   5,  2,  3,  8,  7, 12,
  11,  3,  4, 28,  9,  8,  3, 17, 20, 19, 18, 17,
   4,  4, 21, 27, 28,
   4,  5, 23, 25, 10,
   8,  6,  7,  8,  9, 22, 10, 25, 26,
   9,  1, 24, 23,  5, 21,  4,  3,  2, 29,
   4, 29,  2, 12, 30,
  11, 11, 30, 12,  7,  6, 11, 13, 16, 15, 14, 13,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
   7, 21,  5, 10, 22,  9, 28, 27,
   4, 23, 24, 26, 25,
   5, 24,  1, 11,  6, 26,
   4,  1, 29, 30, 11,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, pt_general    OPAQ  VERT  gyp_gyp_ptn  manager_b      
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
 14, PVmod         TRAN  VERT  PV_constr    EXTERIOR       
# base
  6  0  0  0  0  0    13.50 0
