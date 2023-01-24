# geometry of house defined in: ../zones/house.geo
GEN  house  Simple representation of house  # type, name, descr
      24      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.00000     0.00000     0.00000  # vert   1
     12.00000     0.00000     0.00000  # vert   2
     12.00000    10.00000     0.00000  # vert   3
      4.00000    10.00000     0.00000  # vert   4
      4.00000     0.00000     5.40000  # vert   5
     12.00000     0.00000     5.40000  # vert   6
     12.00000    10.00000     5.40000  # vert   7
      4.00000    10.00000     5.40000  # vert   8
      6.80000     0.00000     1.70000  # vert   9
      9.20000     0.00000     1.70000  # vert  10
      9.20000     0.00000     3.70000  # vert  11
      6.80000     0.00000     3.70000  # vert  12
      9.20000    10.00000     1.70000  # vert  13
      6.80000    10.00000     1.70000  # vert  14
      6.80000    10.00000     3.70000  # vert  15
      9.20000    10.00000     3.70000  # vert  16
     12.00000     3.80000     1.70000  # vert  17
     12.00000     6.20000     1.70000  # vert  18
     12.00000     6.20000     3.70000  # vert  19
     12.00000     3.80000     3.70000  # vert  20
      4.00000     6.20000     1.70000  # vert  21
      4.00000     3.80000     1.70000  # vert  22
      4.00000     3.80000     3.70000  # vert  23
      4.00000     6.20000     3.70000  # vert  24
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,
  10,  2,  3,  7,  6,  2, 17, 20, 19, 18, 17,
  10,  3,  4,  8,  7,  3, 13, 16, 15, 14, 13,
  10,  4,  1,  5,  8,  4, 21, 24, 23, 22, 21,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
   4, 21, 22, 23, 24,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, wall-S        OPAQ  VERT  2x4          EXTERIOR       
  2, wall-E        OPAQ  VERT  2x4          EXTERIOR       
  3, wall-N        OPAQ  VERT  2x4          EXTERIOR       
  4, wall-W        OPAQ  VERT  2x4          EXTERIOR       
  5, ceiling       OPAQ  CEIL  R-30_ceiling EXTERIOR       
  6, slab          OPAQ  FLOR  conc-slab    BASESIMP       
  7, win-S         TRAN  VERT  win-standard EXTERIOR       
  8, win-N         TRAN  VERT  win-standard EXTERIOR       
  9, win-E         TRAN  VERT  win-standard EXTERIOR       
 10, win-W         TRAN  VERT  win-standard EXTERIOR       
# base
  6  0  0  0  0  0    80.00 0
