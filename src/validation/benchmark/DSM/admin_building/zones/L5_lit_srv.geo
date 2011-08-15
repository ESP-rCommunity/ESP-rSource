# geometry of L5_lit_srv defined in: ../zones/L5_lit_srv.geo
GEN  L5_lit_srv  L5_lit_srv describes kitchen at the service facade  # type, name, descr
      12       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     17.00000    30.50000    64.15000  # vert   1
     21.25000    30.50000    64.15000  # vert   2
     21.25000    32.75000    64.15000  # vert   3
     21.25000    36.00000    64.15000  # vert   4
     18.75000    36.00000    64.15000  # vert   5
     17.00000    36.00000    64.15000  # vert   6
     17.00000    30.50000    67.37500  # vert   7
     21.25000    30.50000    67.37500  # vert   8
     21.25000    32.75000    67.37500  # vert   9
     21.25000    36.00000    67.37500  # vert  10
     18.75000    36.00000    67.37500  # vert  11
     17.00000    36.00000    67.37500  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  8,  7,
   4,  2,  3,  9,  8,
   4,  3,  4, 10,  9,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
   4,  6,  1,  7, 12,
   6,  7,  8,  9, 10, 11, 12,
   6,  1,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
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
  7, Top-7         OPAQ  CEIL  UNKNOWN      UNKNOWN        
  8, Base-8        OPAQ  FLOR  UNKNOWN      UNKNOWN        
# base
  8  0  0  0  0  0    23.38 0
