# geometry of main defined in: user/SimpleSqHseL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      35       12 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  2.60000   # vert 1  
    0.00000  6.00000  2.60000   # vert 2  
    0.00000  6.00000  5.62000   # vert 3  
    0.00000  0.00000  5.62000   # vert 4  
    6.00000  6.00000  2.60000   # vert 5  
    6.00000  6.00000  5.62000   # vert 6  
    6.00000  0.00000  2.60000   # vert 7  
    6.00000  0.00000  5.62000   # vert 8  
    1.24900  0.00000  2.60000   # vert 9  
    1.24900  1.24900  2.60000   # vert 10 
    0.00000  1.24900  2.60000   # vert 11 
    0.00000  0.01000  2.61000   # vert 12 
    0.00000  1.31000  2.61000   # vert 13 
    0.00000  1.31000  5.03000   # vert 14 
    0.00000  0.01000  5.03000   # vert 15 
    0.00000  2.01000  3.75000   # vert 16 
    0.00000  3.26000  3.75000   # vert 17 
    0.00000  3.26000  5.00000   # vert 18 
    0.00000  2.01000  5.00000   # vert 19 
    3.51000  6.00000  2.58000   # vert 20 
    5.05000  6.00000  2.58000   # vert 21 
    5.05000  6.00000  5.00000   # vert 22 
    3.51000  6.00000  5.00000   # vert 23 
    6.00000  5.99000  4.00000   # vert 24 
    6.00000  4.99000  4.00000   # vert 25 
    6.00000  4.99000  5.00000   # vert 26 
    6.00000  5.99000  5.00000   # vert 27 
    5.99000  0.00000  2.61000   # vert 28 
    5.07000  0.00000  2.61000   # vert 29 
    5.07000  0.00000  4.71000   # vert 30 
    5.99000  0.00000  4.71000   # vert 31 
    4.99000  0.00000  3.16000   # vert 32 
    2.28000  0.00000  3.16000   # vert 33 
    2.28000  0.00000  5.00000   # vert 34 
    4.99000  0.00000  5.00000   # vert 35 
# no of vertices followed by list of associated vert
  16,   1,   4,   3,   2,   1,  13,  14,  15,  12,  13,   1,  17,  18,  19,  16,  17,
  10,   2,   3,   6,   5,   2,  21,  22,  23,  20,  21,
  10,   5,   6,   8,   7,   5,  25,  26,  27,  24,  25,
  16,   7,   8,   4,   1,   7,  29,  30,  31,  28,  29,   7,  33,  34,  35,  32,  33,
   8,   1,   2,   5,   7,   9,  10,  11,   1,
   4,   8,   6,   3,   4,
   4,  12,  15,  14,  13,
   4,  16,  19,  18,  17,
   4,  20,  23,  22,  21,
   4,  24,  27,  26,  25,
   4,  28,  31,  30,  29,
   4,  32,  35,  34,  33,
# unused index
 0   0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, Wall-W        OPAQ  VERT  ext_wall     EXTERIOR       
  2, Wall-N        OPAQ  VERT  ext_wall     EXTERIOR       
  3, Wall-E        OPAQ  VERT  ext_wall     EXTERIOR       
  4, Wall-S        OPAQ  VERT  ext_wall     EXTERIOR       
  5, to_bsm        OPAQ  FLOR  floors       basement       
  6, to_attic      OPAQ  CEIL  ceiling      attic          
  7, Door-W        OPAQ  VERT  ext_door     EXTERIOR       
  8, Window-W      TRAN  VERT  window       EXTERIOR       
  9, Window-N      TRAN  VERT  window       EXTERIOR       
 10, Window-E      TRAN  VERT  window       EXTERIOR       
 11, Door-S        OPAQ  VERT  ext_door     EXTERIOR       
 12, Window-S      TRAN  VERT  window       EXTERIOR       
# base
5   0   0   0   0   0   36.00    
