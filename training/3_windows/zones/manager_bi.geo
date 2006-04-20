# geometry of manager_bi defined in: ../zones/manager_bi.geo
GEN  manager_bi  manager_bi describes an office with bi-directional opening  # type, name, descr
      24      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.00000     0.00000     0.00000  # vert   1
     11.00000     0.00000     0.00000  # vert   2
     11.00000     4.50000     0.00000  # vert   3
      9.00000     4.50000     0.00000  # vert   4
      8.00000     4.50000     0.00000  # vert   5
      8.00000     0.00000     3.00000  # vert   6
     11.00000     0.00000     3.00000  # vert   7
     11.00000     4.50000     3.00000  # vert   8
      9.00000     4.50000     3.00000  # vert   9
      8.00000     4.50000     3.00000  # vert  10
      8.00000     0.00000     0.90000  # vert  11
     11.00000     0.00000     0.90000  # vert  12
      9.00000     0.00000     0.90000  # vert  13
     10.00000     0.00000     0.90000  # vert  14
      8.10000     0.00000     1.00000  # vert  15
     10.90000     0.00000     1.00000  # vert  16
     10.90000     0.00000     2.90000  # vert  17
      8.10000     0.00000     2.90000  # vert  18
     10.90000     4.50000     0.10000  # vert  19
      9.10000     4.50000     0.10000  # vert  20
      9.10000     4.50000     2.90000  # vert  21
     10.90000     4.50000     2.90000  # vert  22
     10.01600     0.00000     1.00000  # vert  23
     10.01600     0.00000     2.90000  # vert  24
# no of vertices followed by list of associated vert
   5,  2,  3,  8,  7, 12,
  10,  3,  4,  9,  8,  3, 19, 22, 21, 20, 19,
   4,  4,  5, 10,  9,
   5,  5,  1, 11,  6, 10,
   5,  6,  7,  8,  9, 10,
   5,  1,  5,  4,  3,  2,
   6,  1,  2, 12, 14, 13, 11,
  14, 11, 13, 14, 12,  7,  6, 11, 15, 18, 24, 17, 16, 23, 15,
   4, 15, 23, 24, 18,
   4, 19, 20, 21, 22,
   4, 23, 16, 17, 24,
# unused index
  0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
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
 11, bi-glaz       TRAN  VERT  dbl_glz      EXTERIOR       
# base
  6  0  0  0  0  0    13.50
