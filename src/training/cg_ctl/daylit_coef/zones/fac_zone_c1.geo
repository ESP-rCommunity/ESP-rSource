# geometry of fac_zone_c1 defined in: ../zones/fac_zone_c1.geo
GEN  fac_zone_c1  fac_zone_c1 describes a...  # type, name, descr
      22      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      1.00000     1.00000     0.00000  # vert   1
      5.50000     1.00001     0.00000  # vert   2
      5.50000     5.50000     0.00000  # vert   3
      1.00000     5.50000     0.00000  # vert   4
      1.00000     1.00000     3.40000  # vert   5
      5.50000     1.00001     3.40000  # vert   6
      5.50000     5.50000     3.40000  # vert   7
      1.00000     5.50000     3.40000  # vert   8
      1.00000     1.00000     2.80000  # vert   9
      5.50000     1.00001     2.80000  # vert  10
      1.00000     1.00000     0.75000  # vert  11
      5.50000     1.00001     0.75000  # vert  12
      1.00000     1.00000     2.05000  # vert  13
      5.50000     1.00001     2.05000  # vert  14
      1.15000     1.00000     0.90000  # vert  15
      5.35000     1.00000     0.90000  # vert  16
      5.35000     1.00000     1.95000  # vert  17
      1.15000     1.00000     1.95000  # vert  18
      1.15000     1.00000     2.15000  # vert  19
      5.35001     1.00000     2.15000  # vert  20
      5.35000     1.00000     2.70000  # vert  21
      1.15000     1.00000     2.70000  # vert  22
# no of vertices followed by list of associated vert
  10, 11, 12, 14, 13, 11, 15, 18, 17, 16, 15,
   7,  2,  3,  7,  6, 10, 14, 12,
   4,  3,  4,  8,  7,
   7,  4,  1, 11, 13,  9,  5,  8,
   4,  1,  4,  3,  2,
   4,  5,  6,  7,  8,
   4,  9, 10,  6,  5,
   4,  1,  2, 12, 11,
  10, 13, 14, 10,  9, 13, 19, 22, 21, 20, 19,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
# unused index
 0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Frame_bot     OPAQ  VERT  Frame        EXTERIOR       
  2, Right_wall    OPAQ  VERT  Int_wall     SIMILAR        
  3, Rear_wall     OPAQ  VERT  Int_wall     SIMILAR        
  4, Left_wall     OPAQ  VERT  Int_wall     SIMILAR        
  5, Floor         OPAQ  FLOR  Floor        SIMILAR        
  6, Ceiling       OPAQ  CEIL  Ceiling      SIMILAR        
  7, Facade_top    OPAQ  VERT  Facade_top   EXTERIOR       
  8, Facade_bot    OPAQ  VERT  Facade_bot   EXTERIOR       
  9, Frame_top     OPAQ  VERT  Frame        EXTERIOR       
 10, Window_bot    TRAN  VERT  dbl_glz_low  EXTERIOR       
 11, Window_top    TRAN  VERT  dbl_gl_up    EXTERIOR       
# base
  5 12  0  0  0  0    20.25 0
