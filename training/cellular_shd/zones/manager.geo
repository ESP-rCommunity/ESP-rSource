# geometry of manager defined in: ../zones/manager.geo
GEN  manager  manager describes cellular office  # type, name, descr
      22      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      3.00000     0.00000     0.00000  # vert   2
      3.00000     4.50000     0.00000  # vert   3
      1.00000     4.50000     0.00000  # vert   4
      0.00000     4.50000     0.00000  # vert   5
      0.00000     0.00000     3.00000  # vert   6
      3.00000     0.00000     3.00000  # vert   7
      3.00000     4.50000     3.00000  # vert   8
      1.00000     4.50000     3.00000  # vert   9
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
      1.10000     4.50000     0.10000  # vert  20
      1.10000     4.50000     2.90000  # vert  21
      2.90000     4.50000     2.90000  # vert  22
# no of vertices followed by list of associated vert
   5,  2,  3,  8,  7, 12,
  10,  3,  4,  9,  8,  3, 19, 22, 21, 20, 19,
   4,  4,  5, 10,  9,
   5,  5,  1, 11,  6, 10,
   5,  6,  7,  8,  9, 10,
   5,  1,  5,  4,  3,  2,
   6,  1,  2, 12, 14, 13, 11,
  12, 11, 13, 14, 12,  7,  6, 11, 15, 18, 17, 16, 15,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
# unused index
  0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, pt_general    OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
  2, part_frame    OPAQ  VERT  insul_frame  SIMILAR        
  3, door          OPAQ  VERT  door         SIMILAR        
  4, pt_other      OPAQ  VERT  gyp_gyp_ptn  SIMILAR        
  5, ceiling       OPAQ  CEIL  ceiling      SIMILAR        
  6, floor         OPAQ  FLOR  susp_flr_re  SIMILAR        
  7, spandral      OPAQ  VERT  insul_frame  EXTERIOR       
  8, frame         OPAQ  VERT  insul_frame  EXTERIOR       
  9, glazing       TRAN  VERT  dbl_glz      EXTERIOR       
 10, part_glaz     TRAN  VERT  dbl_glz      SIMILAR        
# base
  6  0  0  0  0  0    13.50
