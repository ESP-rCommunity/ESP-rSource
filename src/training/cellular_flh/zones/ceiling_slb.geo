# geometry of ceiling_slb defined in: ../zones/ceiling_slb.geo
GEN  ceiling_slb  ceiling_slb represents zone for heating at ceiling level  # type, name, descr
      20       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     3.20000  # vert   1
      6.10000     0.00000     3.20000  # vert   2
      6.10000     5.55000     3.20000  # vert   3
      0.00000     5.55000     3.20000  # vert   4
      0.00000     0.00000     3.00000  # vert   5
      6.10000     0.00000     3.00000  # vert   6
      6.10000     5.55000     3.00000  # vert   7
      0.00000     5.55000     3.00000  # vert   8
      0.00000     2.35000     3.00000  # vert   9
      0.00000     2.50000     3.00000  # vert  10
      0.00000     4.50000     3.00000  # vert  11
      3.00000     4.50000     3.00000  # vert  12
      3.00000     0.00000     3.00000  # vert  13
      6.10000     2.35000     3.00000  # vert  14
      3.10000     0.00000     3.00000  # vert  15
      3.10000     4.50000     3.00000  # vert  16
      6.10000     4.50000     3.00000  # vert  17
      6.10000     2.50000     3.00000  # vert  18
      0.00000     4.55000     3.00000  # vert  19
      6.10000     4.55000     3.00000  # vert  20
# no of vertices followed by list of associated vert
   6,  2,  1,  5, 13, 15,  6,
   8,  3,  2,  6, 14, 18, 17, 20,  7,
   4,  4,  3,  7,  8,
   8,  1,  4,  8, 19, 11, 10,  9,  5,
   4,  4,  1,  2,  3,
   6,  5,  9, 10, 11, 12, 13,
   6, 14,  6, 15, 16, 17, 18,
   4, 19,  8,  7, 20,
   8, 15, 13, 12, 11, 19, 20, 17, 16,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, edge_s        OPAQ  VERT  extern_wall  SIMILAR        
  2, edge_e        OPAQ  VERT  structure    SIMILAR        
  3, edge_n        OPAQ  VERT  structure    SIMILAR        
  4, edge_w        OPAQ  VERT  structure    SIMILAR        
  5, upper         OPAQ  CEIL  screed_rev   CONSTANT       
  6, floor_ma      OPAQ  FLOR  ceiling_scrd manager_a      
  7, floor_mb      OPAQ  FLOR  ceiling_scrd manager_b      
  8, floor_cor     OPAQ  FLOR  ceiling_scrd coridor        
  9, filler        OPAQ  FLOR  screed_rev   ADIABATIC      
# base
  6  5  7  8  9  0    67.71 0
