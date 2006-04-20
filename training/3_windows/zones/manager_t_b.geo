# geometry of manager_t_b defined in: ../zones/manager_t_b.geo
GEN  manager_t_b  manager_t_b describes an office with top and bottom openings  # type, name, descr
      26      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.00000     0.00000     0.00000  # vert   1
      7.00000     0.00000     0.00000  # vert   2
      7.00000     4.50000     0.00000  # vert   3
      5.00000     4.50000     0.00000  # vert   4
      4.00000     4.50000     0.00000  # vert   5
      4.00000     0.00000     3.00000  # vert   6
      7.00000     0.00000     3.00000  # vert   7
      7.00000     4.50000     3.00000  # vert   8
      5.00000     4.50000     3.00000  # vert   9
      4.00000     4.50000     3.00000  # vert  10
      4.00000     0.00000     0.90000  # vert  11
      7.00000     0.00000     0.90000  # vert  12
      5.00000     0.00000     0.90000  # vert  13
      6.00000     0.00000     0.90000  # vert  14
      4.10000     0.00000     1.00000  # vert  15
      6.90000     0.00000     1.00000  # vert  16
      6.90000     0.00000     2.90000  # vert  17
      4.10000     0.00000     2.90000  # vert  18
      6.90000     4.50000     0.10000  # vert  19
      5.10000     4.50000     0.10000  # vert  20
      5.10000     4.50000     2.90000  # vert  21
      6.90000     4.50000     2.90000  # vert  22
      4.10000     0.00000     1.30000  # vert  23
      6.90000     0.00000     1.30000  # vert  24
      6.90000     0.00000     2.60000  # vert  25
      4.10000     0.00000     2.60000  # vert  26
# no of vertices followed by list of associated vert
   5,  2,  3,  8,  7, 12,
  10,  3,  4,  9,  8,  3, 19, 22, 21, 20, 19,
   4,  4,  5, 10,  9,
   5,  5,  1, 11,  6, 10,
   5,  6,  7,  8,  9, 10,
   5,  1,  5,  4,  3,  2,
   6,  1,  2, 12, 14, 13, 11,
  16, 11, 13, 14, 12,  7,  6, 11, 15, 23, 26, 18, 17, 25, 24, 16, 15,
   4, 23, 24, 25, 26,
   4, 19, 20, 21, 22,
   4, 15, 16, 24, 23,
   4, 26, 25, 17, 18,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
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
  7, spandral      OPAQ  VERT  extern_wall  EXTERIOR       
  8, frame         OPAQ  VERT  insul_frame  EXTERIOR       
  9, glazing       TRAN  VERT  dbl_glz      EXTERIOR       
 10, part_glaz     TRAN  VERT  dbl_glz      SIMILAR        
 11, low_glz       TRAN  VERT  dbl_glz      EXTERIOR       
 12, high_glz      TRAN  VERT  dbl_glz      EXTERIOR       
# base
  6  0  0  0  0  0    13.50
