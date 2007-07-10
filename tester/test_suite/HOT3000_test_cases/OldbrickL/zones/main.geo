# geometry of main defined in: user/OldbrickL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      31       14 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  2.50000   # vert 1  
    0.00000  9.05000  2.50000   # vert 2  
    0.00000  9.05000  5.00000   # vert 3  
    0.00000  0.00000  5.00000   # vert 4  
    9.05000  9.05000  2.50000   # vert 5  
    9.05000  9.05000  5.00000   # vert 6  
    9.05000  0.00000  2.50000   # vert 7  
    9.05000  0.00000  5.00000   # vert 8  
    0.00000  9.05000  7.50000   # vert 9  
    0.00000  0.00000  7.50000   # vert 10 
    9.05000  9.05000  7.50000   # vert 11 
    9.05000  0.00000  7.50000   # vert 12 
    1.24900  0.00000  2.50000   # vert 13 
    1.24900  1.24900  2.50000   # vert 14 
    0.00000  1.24900  2.50000   # vert 15 
    0.00000  0.01000  2.51000   # vert 16 
    0.00000  0.70000  2.51000   # vert 17 
    0.00000  0.70000  4.51000   # vert 18 
    0.00000  0.01000  4.51000   # vert 19 
    0.00000  2.01000  3.00000   # vert 20 
    0.00000  5.11000  3.00000   # vert 21 
    0.00000  5.11000  4.50000   # vert 22 
    0.00000  2.01000  4.50000   # vert 23 
    9.05000  8.04000  3.00000   # vert 24 
    9.05000  4.34000  3.00000   # vert 25 
    9.05000  4.34000  4.50000   # vert 26 
    9.05000  8.04000  4.50000   # vert 27 
    8.04000  0.00000  3.00000   # vert 28 
    5.56000  0.00000  3.00000   # vert 29 
    5.56000  0.00000  4.50000   # vert 30 
    8.04000  0.00000  4.50000   # vert 31 
# no of vertices followed by list of associated vert
  16,   1,   4,   3,   2,   1,  17,  18,  19,  16,  17,   1,  21,  22,  23,  20,  21,
   4,   2,   3,   6,   5,
  10,   5,   6,   8,   7,   5,  25,  26,  27,  24,  25,
  10,   7,   8,   4,   1,   7,  29,  30,  31,  28,  29,
   4,   4,  10,   9,   3,
   4,   3,   9,  11,   6,
   4,   6,  11,  12,   8,
   4,   8,  12,  10,   4,
   8,   1,   2,   5,   7,  13,  14,  15,   1,
   4,   8,   6,   3,   4,
   4,  16,  19,  18,  17,
   4,  20,  23,  22,  21,
   4,  24,  27,  26,  25,
   4,  28,  31,  30,  29,
# unused index
 0   0   0   0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, Wall-W        OPAQ  VERT  ext_wall     EXTERIOR       
  2, Wall-N        OPAQ  VERT  ext_wall     EXTERIOR       
  3, Wall-E        OPAQ  VERT  ext_wall     EXTERIOR       
  4, Wall-S        OPAQ  VERT  ext_wall     EXTERIOR       
  5, Wall-W2       OPAQ  VERT  ext_wall     EXTERIOR       
  6, Wall-N2       OPAQ  VERT  ext_wall     EXTERIOR       
  7, Wall-E2       OPAQ  VERT  ext_wall     EXTERIOR       
  8, Wall-S2       OPAQ  VERT  ext_wall     EXTERIOR       
  9, to_bsm        OPAQ  FLOR  floors       Foundation     
 10, to_attic      OPAQ  CEIL  ceiling      attic          
 11, Door          OPAQ  VERT  ext_door     EXTERIOR       
 12, Window-W      TRAN  VERT  window       EXTERIOR       
 13, Window-E      TRAN  VERT  window       EXTERIOR       
 14, Window-S      TRAN  VERT  window       EXTERIOR       
# base
9   0   0   0   0   0   81.90    
