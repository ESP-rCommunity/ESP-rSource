# geometry of floor_below defined in: ../zones/floor_below.geo
GEN  floor_below  floor_below is a thin zone representing ceiling void below  # type, name, descr
      20       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000    -0.50000  # vert   1
      6.10000     0.00000    -0.50000  # vert   2
      6.10000     5.55000    -0.50000  # vert   3
      0.00000     5.55000    -0.50000  # vert   4
      0.00000     0.00000     0.00000  # vert   5
      6.10000     0.00000     0.00000  # vert   6
      6.10000     5.55000     0.00000  # vert   7
      0.00000     5.55000     0.00000  # vert   8
      0.00000     2.35000     0.00000  # vert   9
      0.00000     2.50000     0.00000  # vert  10
      0.00000     4.50000     0.00000  # vert  11
      3.00000     4.50000     0.00000  # vert  12
      3.00000     0.00000     0.00000  # vert  13
      6.10000     2.35000     0.00000  # vert  14
      3.10000     0.00000     0.00000  # vert  15
      3.10000     4.50000     0.00000  # vert  16
      6.10000     4.50000     0.00000  # vert  17
      6.10000     2.50000     0.00000  # vert  18
      0.00000     4.55000     0.00000  # vert  19
      6.10000     4.55000     0.00000  # vert  20
# no of vertices followed by list of associated vert
   6,  1,  2,  6, 15, 13,  5,
   8,  2,  3,  7, 20, 17, 18, 14,  6,
   4,  3,  4,  8,  7,
   8,  4,  1,  5,  9, 10, 11, 19,  8,
   4,  1,  4,  3,  2,
   6,  9,  5, 13, 12, 11, 10,
   6,  6, 14, 18, 17, 16, 15,
   4,  8, 19, 20,  7,
   8, 13, 15, 16, 17, 20, 19, 11, 12,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, edge_s        OPAQ  VERT  extern_wall  EXTERIOR       
  2, edge_e        OPAQ  VERT  structure    SIMILAR        
  3, edge_n        OPAQ  VERT  structure    SIMILAR        
  4, edge_w        OPAQ  VERT  structure    SIMILAR        
  5, Surf-6        OPAQ  FLOR  ceiling_rev  CONSTANT       
  6, floor_ma      OPAQ  CEIL  structure    manager_a      
  7, floor_mb      OPAQ  CEIL  structure    manager_b      
  8, floor_cor     OPAQ  CEIL  structure    coridor        
  9, filler        OPAQ  CEIL  structure    ADIABATIC      
# base
  6  5  0  0  0  0    47.35 0
