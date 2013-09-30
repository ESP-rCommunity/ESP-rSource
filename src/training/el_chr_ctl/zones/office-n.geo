# geometry of office-n defined in: ../zones/office-n.geo
GEN  office-n  office-n describes a...  # type, name, descr
      30      17   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      5.50000    14.50000    30.00000  # vert   1
     10.00000    14.49999    30.00000  # vert   2
     10.00000    10.00000    30.00000  # vert   3
      5.50000    10.00000    30.00000  # vert   4
      5.50000    14.50000    33.00000  # vert   5
     10.00000    14.49999    33.00000  # vert   6
     10.00000    10.00000    33.00000  # vert   7
      5.50000    10.00000    33.00000  # vert   8
      5.50000    14.50000    31.00000  # vert   9
     10.00000    14.49999    31.00000  # vert  10
      5.50000    14.50000    31.10000  # vert  11
      5.50000    14.50000    32.90000  # vert  12
     10.00000    14.50000    31.10000  # vert  13
     10.00000    14.50000    32.90000  # vert  14
      5.60000    14.50000    31.10000  # vert  15
      6.60000    14.50000    31.10000  # vert  16
      5.60000    14.50000    32.90000  # vert  17
      6.60000    14.50000    32.90000  # vert  18
      6.70000    14.50000    31.10000  # vert  19
      7.70000    14.50000    31.10000  # vert  20
      6.70000    14.50000    32.90000  # vert  21
      7.70000    14.50000    32.90000  # vert  22
      7.80000    14.50000    31.10000  # vert  23
      8.80000    14.50000    31.10000  # vert  24
      7.80000    14.50000    32.90000  # vert  25
      8.80000    14.50000    32.90000  # vert  26
      8.90000    14.50000    31.10000  # vert  27
      8.90000    14.50000    32.90000  # vert  28
      9.90000    14.50000    31.10000  # vert  29
      9.90000    14.50000    32.90000  # vert  30
# no of vertices followed by list of associated vert
   7,  3,  2, 10, 13, 14,  6,  7,
   4,  4,  3,  7,  8,
   7,  1,  4,  8,  5, 12, 11,  9,
   4,  4,  1,  2,  3,
   4,  6,  5,  8,  7,
   4,  2,  1,  9, 10,
  12, 10,  9, 11, 15, 16, 19, 20, 23, 24, 27, 29, 13,
  12, 17, 12,  5,  6, 14, 30, 28, 26, 25, 22, 21, 18,
   4, 15, 11, 12, 17,
   4, 19, 16, 18, 21,
   4, 23, 20, 22, 25,
   4, 27, 24, 26, 28,
   4, 13, 29, 30, 14,
   4, 16, 15, 17, 18,
   4, 20, 19, 21, 22,
   4, 24, 23, 25, 26,
   4, 29, 27, 28, 30,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
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
  4 12  0  0  0  0    20.43 0
