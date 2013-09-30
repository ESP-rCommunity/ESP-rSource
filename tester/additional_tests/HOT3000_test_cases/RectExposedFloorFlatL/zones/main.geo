# geometry of main defined in: user/RectExposedFloorFlatL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      28       11 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  0.00000   # vert 1  
    0.00000  10.00000  0.00000   # vert 2  
    0.00000  10.00000  2.50000   # vert 3  
    0.00000  0.00000  2.50000   # vert 4  
    10.00000  10.00000  0.00000   # vert 5  
    10.00000  10.00000  2.50000   # vert 6  
    10.00000  0.00000  0.00000   # vert 7  
    10.00000  0.00000  2.50000   # vert 8  
    0.00000  2.01000  0.00000   # vert 9  
    0.00000  6.01000  0.00000   # vert 10 
    0.00000  6.01000  2.00000   # vert 11 
    0.00000  2.01000  2.00000   # vert 12 
    2.01000  10.00000  0.00000   # vert 13 
    6.01000  10.00000  0.00000   # vert 14 
    6.01000  10.00000  2.00000   # vert 15 
    2.01000  10.00000  2.00000   # vert 16 
    10.00000  9.99000  0.01000   # vert 17 
    10.00000  9.18000  0.01000   # vert 18 
    10.00000  9.18000  2.08000   # vert 19 
    10.00000  9.99000  2.08000   # vert 20 
    10.00000  7.99000  0.00000   # vert 21 
    10.00000  3.99000  0.00000   # vert 22 
    10.00000  3.99000  2.00000   # vert 23 
    10.00000  7.99000  2.00000   # vert 24 
    7.99000  0.00000  0.00000   # vert 25 
    3.99000  0.00000  0.00000   # vert 26 
    3.99000  0.00000  2.00000   # vert 27 
    7.99000  0.00000  2.00000   # vert 28 
# no of vertices followed by list of associated vert
  10,   1,   4,   3,   2,   1,  10,  11,  12,   9,  10,
  10,   2,   3,   6,   5,   2,  14,  15,  16,  13,  14,
  16,   5,   6,   8,   7,   5,  18,  19,  20,  17,  18,   5,  22,  23,  24,  21,  22,
  10,   7,   8,   4,   1,   7,  26,  27,  28,  25,  26,
   4,   1,   2,   5,   7,
   4,   8,   6,   3,   4,
   4,   9,  12,  11,  10,
   4,  13,  16,  15,  14,
   4,  17,  20,  19,  18,
   4,  21,  24,  23,  22,
   4,  25,  28,  27,  26,
# unused index
 0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, MainWall-W    OPAQ  VERT  ext_wall     EXTERIOR       
  2, MainWall-N    OPAQ  VERT  ext_wall     EXTERIOR       
  3, MainWall-E    OPAQ  VERT  ext_wall     EXTERIOR       
  4, MainWall-S    OPAQ  VERT  ext_wall     EXTERIOR       
  5, floor         OPAQ  FLOR  ext_floor    EXTERIOR       
  6, FlatRoof      OPAQ  FLAT  asphalt      EXTERIOR       
  7, Window-32     TRAN  VERT  window       EXTERIOR       
  8, Window-30     TRAN  VERT  window       EXTERIOR       
  9, Door-5        OPAQ  VERT  ext_door     EXTERIOR       
 10, Window-29     TRAN  VERT  window       EXTERIOR       
 11, Window-31     TRAN  VERT  window       EXTERIOR       
# base
5   0   0   0   0   0   100.00   
