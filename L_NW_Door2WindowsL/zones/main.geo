# geometry of main defined in: user/L_NW_Door2WindowsL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      23       11 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  2.50000   # vert 1  
    0.00000  8.60000  2.50000   # vert 2  
    0.00000  8.60000  4.94000   # vert 3  
    0.00000  0.00000  4.94000   # vert 4  
    10.00000  8.60000  2.50000   # vert 5  
    10.00000  8.60000  4.94000   # vert 6  
    10.00000  4.00000  2.50000   # vert 7  
    10.00000  4.00000  4.94000   # vert 8  
    4.00000  4.00000  2.50000   # vert 9  
    4.00000  4.00000  4.94000   # vert 10 
    4.00000  0.00000  2.50000   # vert 11 
    4.00000  0.00000  4.94000   # vert 12 
    0.01000  8.60000  2.51000   # vert 13 
    1.01000  8.60000  2.51000   # vert 14 
    1.01000  8.60000  4.51000   # vert 15 
    0.01000  8.60000  4.51000   # vert 16 
    2.01000  8.60000  3.90000   # vert 17 
    3.01000  8.60000  3.90000   # vert 18 
    3.01000  8.60000  4.90000   # vert 19 
    2.01000  8.60000  4.90000   # vert 20 
    3.01000  8.60000  4.40000   # vert 21 
    3.51000  8.60000  4.40000   # vert 22 
    3.51000  8.60000  4.90000   # vert 23 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
  22,   2,   3,   6,   5,   2,  14,  15,  16,  13,  14,   2,  18,  19,  20,  17,  18,   2,  22,  23,  19,  21,  22,
   4,   5,   6,   8,   7,
   4,   7,   8,  10,   9,
   4,   9,  10,  12,  11,
   4,  11,  12,   4,   1,
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
  1, MainWall-02   OPAQ  VERT  ext_wall     EXTERIOR       
  2, MainWall-01   OPAQ  VERT  ext_wall     EXTERIOR       
  3, MainWall-04   OPAQ  VERT  ext_wall     EXTERIOR       
  4, MainWall-03   OPAQ  VERT  ext_wall     EXTERIOR       
  5, MainWall-06   OPAQ  VERT  ext_wall     EXTERIOR       
  6, MainWall-05   OPAQ  VERT  ext_wall     EXTERIOR       
  7, to_bsm        OPAQ  FLOR  floors       Foundation     
  8, to_attic      OPAQ  CEIL  ceiling      attic          
  9, Door-3        OPAQ  VERT  ext_door     EXTERIOR       
 10, Window-25     TRAN  VERT  window       EXTERIOR       
 11, Window-26     TRAN  VERT  window       EXTERIOR       
# base
7   0   0   0   0   0   62.00    
