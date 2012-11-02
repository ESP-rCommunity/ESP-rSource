# geometry of lowkitchen defined in: ../zones/lowkitchen.geo
GEN  lowkitchen  lowkitchen describes corner kitchen of lower flat  # type, name, descr
      20      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.60000     2.60000     0.20000  # vert   1
      9.60000     2.60000     0.20000  # vert   2
      9.60000     7.30000     0.20000  # vert   3
      8.60000     7.30000     0.20000  # vert   4
      7.00000     7.30000     0.20000  # vert   5
      6.10000     7.30000     0.20000  # vert   6
      6.10000     4.40000     0.20000  # vert   7
      6.10000     2.60000     0.20000  # vert   8
      6.50000     2.60000     0.20000  # vert   9
      7.40000     2.60000     0.20000  # vert  10
      8.60000     2.60000     2.40000  # vert  11
      9.60000     2.60000     2.40000  # vert  12
      9.60000     7.30000     2.40000  # vert  13
      8.60000     7.30000     2.40000  # vert  14
      7.00000     7.30000     2.40000  # vert  15
      6.10000     7.30000     2.40000  # vert  16
      6.10000     4.40000     2.40000  # vert  17
      6.10000     2.60000     2.40000  # vert  18
      6.50000     2.60000     2.40000  # vert  19
      7.40000     2.60000     2.40000  # vert  20
# no of vertices followed by list of associated vert
   4,  1,  2, 12, 11,
   4,  2,  3, 13, 12,
   4,  3,  4, 14, 13,
   4,  4,  5, 15, 14,
   4,  5,  6, 16, 15,
   4,  6,  7, 17, 16,
   4,  7,  8, 18, 17,
   4,  8,  9, 19, 18,
   4,  9, 10, 20, 19,
   4, 10,  1, 11, 20,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  10,  1, 10,  9,  8,  7,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
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
  9, Wall-9        OPAQ  VERT  UNKNOWN      UNKNOWN        
 10, Wall-10       OPAQ  VERT  UNKNOWN      UNKNOWN        
 11, Top-11        OPAQ  CEIL  UNKNOWN      UNKNOWN        
 12, Base-12       OPAQ  FLOR  UNKNOWN      UNKNOWN        
# base
 12  0  0  0  0  0    16.45 0
