# geometry of stair defined in: ../zones/stair.geo
GEN  stair  stair describes stair and space above it  # type, name, descr
      16      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.50000     4.90000     0.20000  # vert   1
      6.50000     7.00000     0.20000  # vert   2
      5.90000     7.00000     0.20000  # vert   3
      5.10000     7.00000     0.20000  # vert   4
      4.50000     7.00000     0.20000  # vert   5
      4.50000     6.10000     0.20000  # vert   6
      4.50000     4.90000     0.20000  # vert   7
      5.50000     4.90000     0.20000  # vert   8
      6.50000     4.90000     2.60000  # vert   9
      6.50000     7.00000     2.60000  # vert  10
      5.90000     7.00000     2.60000  # vert  11
      5.10000     7.00000     2.60000  # vert  12
      4.50000     7.00000     2.60000  # vert  13
      4.50000     6.10000     2.60000  # vert  14
      4.50000     4.90000     2.60000  # vert  15
      5.50000     4.90000     2.60000  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2, 10,  9,
   4,  2,  3, 11, 10,
   4,  3,  4, 12, 11,
   4,  4,  5, 13, 12,
   4,  5,  6, 14, 13,
   4,  6,  7, 15, 14,
   4,  7,  8, 16, 15,
   4,  8,  1,  9, 16,
   8,  9, 10, 11, 12, 13, 14, 15, 16,
   8,  1,  8,  7,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Wall-1        OPAQ  VERT  UNKNOWN      UNKNOWN        
  2, Wall-2        OPAQ  VERT  UNKNOWN      UNKNOWN        
  3, Wall-3        OPAQ  VERT  UNKNOWN      UNKNOWN        
  4, Wall-4        OPAQ  VERT  UNKNOWN      UNKNOWN        
  5, Wall-5        OPAQ  VERT  UNKNOWN      UNKNOWN        
  6, Wall-6        OPAQ  VERT  UNKNOWN      UNKNOWN        
  7, Wall-7        OPAQ  VERT  UNKNOWN      UNKNOWN        
  8, Wall-8        OPAQ  VERT  UNKNOWN      UNKNOWN        
  9, Top-9         OPAQ  CEIL  UNKNOWN      UNKNOWN        
 10, Base-10       OPAQ  FLOR  UNKNOWN      UNKNOWN        
# base
 10  0  0  0  0  0     4.20 0
