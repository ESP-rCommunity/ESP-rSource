# geometry of main defined in: user/L_SW_Door2WindowsL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      23       11 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  2.44000   # vert 1  
    0.00000  8.60000  2.44000   # vert 2  
    0.00000  8.60000  4.88000   # vert 3  
    0.00000  0.00000  4.88000   # vert 4  
    6.00000  8.60000  2.44000   # vert 5  
    6.00000  8.60000  4.88000   # vert 6  
    6.00000  4.00000  2.44000   # vert 7  
    6.00000  4.00000  4.88000   # vert 8  
    10.00000  4.00000  2.44000   # vert 9  
    10.00000  4.00000  4.88000   # vert 10 
    10.00000  0.00000  2.44000   # vert 11 
    10.00000  0.00000  4.88000   # vert 12 
    9.99000  0.00000  2.45000   # vert 13 
    8.99000  0.00000  2.45000   # vert 14 
    8.99000  0.00000  4.45000   # vert 15 
    9.99000  0.00000  4.45000   # vert 16 
    7.99000  0.00000  3.44000   # vert 17 
    6.99000  0.00000  3.44000   # vert 18 
    6.99000  0.00000  4.44000   # vert 19 
    7.99000  0.00000  4.44000   # vert 20 
    6.99000  0.00000  3.94000   # vert 21 
    6.49000  0.00000  3.94000   # vert 22 
    6.49000  0.00000  4.44000   # vert 23 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
   4,   2,   3,   6,   5,
   4,   5,   6,   8,   7,
   4,   7,   8,  10,   9,
   4,   9,  10,  12,  11,
  22,  11,  12,   4,   1,  11,  14,  15,  16,  13,  14,  11,  18,  19,  20,  17,  18,  11,  22,  23,  19,  21,  22,
   6,   1,   2,   5,   7,   9,  11,
   6,  12,  10,   8,   6,   3,   4,
   4,  13,  16,  15,  14,
   4,  17,  20,  19,  18,
   4,  21,  19,  23,  22,
# unused index
 0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, MainWall-W    OPAQ  VERT  ext_wall     EXTERIOR       
  2, MainWall-N1   OPAQ  VERT  ext_wall     EXTERIOR       
  3, MainWall-E1   OPAQ  VERT  ext_wall     EXTERIOR       
  4, MainWall-N2   OPAQ  VERT  ext_wall     EXTERIOR       
  5, MainWall-E2   OPAQ  VERT  ext_wall     EXTERIOR       
  6, MainWall-S    OPAQ  VERT  ext_wall     EXTERIOR       
  7, to_bsm        OPAQ  FLOR  floors       Foundation     
  8, to_attic      OPAQ  CEIL  ceiling      attic          
  9, Door-3        OPAQ  VERT  ext_door     EXTERIOR       
 10, Window-25     TRAN  VERT  window       EXTERIOR       
 11, Window-26     TRAN  VERT  window       EXTERIOR       
# base
7   0   0   0   0   0   67.60    
