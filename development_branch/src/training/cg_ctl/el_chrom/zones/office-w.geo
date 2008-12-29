# geometry of office-w defined in: ../zones/office-w.geo
GEN  office-w               # type   zone name
      30      17   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      1.00000    10.00000    30.00000  # vert   1
      1.00001     5.50000    30.00000  # vert   2
      5.50000     5.50000    30.00000  # vert   3
      5.50000    10.00000    30.00000  # vert   4
      1.00000    10.00000    33.00000  # vert   5
      1.00001     5.50000    33.00000  # vert   6
      5.50000     5.50000    33.00000  # vert   7
      5.50000    10.00000    33.00000  # vert   8
      1.00000    10.00000    31.00000  # vert   9
      1.00001     5.50000    31.00000  # vert  10
      1.00000    10.00000    31.10000  # vert  11
      1.00000    10.00000    32.90000  # vert  12
      1.00000     5.50000    31.10000  # vert  13
      1.00000     5.50000    32.90000  # vert  14
      1.00000     9.90000    31.10000  # vert  15
      1.00000     8.90000    31.10000  # vert  16
      1.00000     9.90000    32.90000  # vert  17
      1.00000     8.90000    32.90000  # vert  18
      1.00000     8.80000    31.10000  # vert  19
      1.00000     7.80000    31.10000  # vert  20
      1.00000     8.80000    32.90000  # vert  21
      1.00000     7.80000    32.90000  # vert  22
      1.00000     7.70000    31.10000  # vert  23
      1.00000     6.70000    31.10000  # vert  24
      1.00000     7.70000    32.90000  # vert  25
      1.00000     6.70000    32.90000  # vert  26
      1.00000     6.60000    31.10000  # vert  27
      1.00000     6.60000    32.90000  # vert  28
      1.00000     5.60000    31.10000  # vert  29
      1.00000     5.60000    32.90000  # vert  30
# no of vertices followed by list of associated vert
   7,  2,  3,  7,  6, 14, 13, 10,
   4,  3,  4,  8,  7,
   7,  4,  1,  9, 11, 12,  5,  8,
   4,  1,  4,  3,  2,
   4,  5,  6,  7,  8,
   4,  1,  2, 10,  9,
  12,  9, 10, 13, 29, 27, 24, 23, 20, 19, 16, 15, 11,
  12, 12, 17, 18, 21, 22, 25, 26, 28, 30, 14,  6,  5,
   4, 11, 15, 17, 12,
   4, 16, 19, 21, 18,
   4, 20, 23, 25, 22,
   4, 24, 27, 28, 26,
   4, 29, 13, 14, 30,
   4, 15, 16, 18, 17,
   4, 19, 20, 22, 21,
   4, 23, 24, 26, 25,
   4, 27, 29, 30, 28,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Right_wall    OPAQ  VERT  Int_wall     SIMILAR        
  2, Rear_wall     OPAQ  VERT  Int_wall     SIMILAR        
  3, Left_wall     OPAQ  VERT  Int_wall     SIMILAR        
  4, Floor         OPAQ  FLOR  Floor        SIMILAR        
  5, Ceiling       OPAQ  CEIL  Ceiling      SIMILAR        
  6, Parapet       OPAQ  VERT  Sparel-pan   EXTERIOR       
  7, Frame-bot     OPAQ  VERT  Frame        EXTERIOR       
  8, Frame-top     OPAQ  VERT  Frame        EXTERIOR       
  9, Frame-1       OPAQ  VERT  Frame        EXTERIOR       
 10, Frame-2       OPAQ  VERT  Frame        EXTERIOR       
 11, Frame-3       OPAQ  VERT  Frame        EXTERIOR       
 12, Frame-4       OPAQ  VERT  Frame        EXTERIOR       
 13, Frame-5       OPAQ  VERT  Frame        EXTERIOR       
 14, Window-1      TRAN  VERT  Glazing      EXTERIOR       
 15, Window-2      TRAN  VERT  Glazing      EXTERIOR       
 16, Window-3      TRAN  VERT  Glazing      EXTERIOR       
 17, Window-4      TRAN  VERT  Glazing      EXTERIOR       
# base
   4  12   0   0    20.25
