# geometry of main defined in: user/L_NE_Door2WindowsL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      28       12 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  5.00000  3.00000   # vert 1  
    0.00000  10.00000  3.00000   # vert 2  
    0.00000  10.00000  5.50000   # vert 3  
    0.00000  5.00000  5.50000   # vert 4  
    10.00000  10.00000  3.00000   # vert 5  
    10.00000  10.00000  5.50000   # vert 6  
    10.00000  0.00000  3.00000   # vert 7  
    10.00000  0.00000  5.50000   # vert 8  
    5.00000  0.00000  3.00000   # vert 9  
    5.00000  0.00000  5.50000   # vert 10 
    5.00000  5.00000  3.00000   # vert 11 
    5.00000  5.00000  5.50000   # vert 12 
    0.01000  10.00000  3.01000   # vert 13 
    1.01000  10.00000  3.01000   # vert 14 
    1.01000  10.00000  5.01000   # vert 15 
    0.01000  10.00000  5.01000   # vert 16 
    0.01000  10.00000  3.75000   # vert 17 
    0.51000  10.00000  3.75000   # vert 18 
    0.51000  10.00000  4.25000   # vert 19 
    0.01000  10.00000  4.25000   # vert 20 
    2.01000  10.00000  4.40000   # vert 21 
    3.01000  10.00000  4.40000   # vert 22 
    3.01000  10.00000  5.40000   # vert 23 
    2.01000  10.00000  5.40000   # vert 24 
    10.00000  9.99000  4.40000   # vert 25 
    10.00000  8.99000  4.40000   # vert 26 
    10.00000  8.99000  5.40000   # vert 27 
    10.00000  9.99000  5.40000   # vert 28 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
  16,   2,   3,   6,   5,   2,  14,  15,  16,  13,  14,   2,  22,  23,  24,  21,  22,
  10,   5,   6,   8,   7,   5,  26,  27,  28,  25,  26,
   4,   7,   8,  10,   9,
   4,   9,  10,  12,  11,
   4,  11,  12,   4,   1,
   6,   1,   2,   5,   7,   9,  11,
   6,  12,  10,   8,   6,   3,   4,
  11,  13,  16,  15,  14,   2,  18,  19,  20,  17,  18,   2,
   4,  17,  20,  19,  18,
   4,  21,  24,  23,  22,
   4,  25,  28,  27,  26,
# unused index
 0   0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, MainWall-W1   OPAQ  VERT  ext_wall     EXTERIOR       
  2, MainWall-N    OPAQ  VERT  ext_wall     EXTERIOR       
  3, MainWall-E    OPAQ  VERT  ext_wall     EXTERIOR       
  4, MainWall-S1   OPAQ  VERT  ext_wall     EXTERIOR       
  5, MainWall-W2   OPAQ  VERT  ext_wall     EXTERIOR       
  6, MainWall-S2   OPAQ  VERT  ext_wall     EXTERIOR       
  7, to_bsm        OPAQ  FLOR  floors       Foundation     
  8, to_attic      OPAQ  CEIL  ceiling      attic          
  9, Door-3        OPAQ  VERT  ext_door     EXTERIOR       
 10, Window-27     TRAN  VERT  window       EXTERIOR       
 11, WindowN       TRAN  VERT  window       EXTERIOR       
 12, WindowS1      TRAN  VERT  window       EXTERIOR       
# base
7   0   0   0   0   0   75.00    
