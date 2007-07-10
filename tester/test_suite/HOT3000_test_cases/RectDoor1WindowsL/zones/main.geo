# geometry of main defined in: user/RectDoor1WindowsL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      16        8 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  2.44000   # vert 1  
    0.00000  8.60000  2.44000   # vert 2  
    0.00000  8.60000  5.04000   # vert 3  
    0.00000  0.00000  5.04000   # vert 4  
    10.00000  8.60000  2.44000   # vert 5  
    10.00000  8.60000  5.04000   # vert 6  
    10.00000  0.00000  2.44000   # vert 7  
    10.00000  0.00000  5.04000   # vert 8  
    9.99000  0.00000  2.45000   # vert 9  
    8.99000  0.00000  2.45000   # vert 10 
    8.99000  0.00000  4.45000   # vert 11 
    9.99000  0.00000  4.45000   # vert 12 
    7.99000  0.00000  3.94000   # vert 13 
    7.49000  0.00000  3.94000   # vert 14 
    7.49000  0.00000  4.44000   # vert 15 
    7.99000  0.00000  4.44000   # vert 16 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
   4,   2,   3,   6,   5,
   4,   5,   6,   8,   7,
  16,   7,   8,   4,   1,   7,  10,  11,  12,   9,  10,   7,  14,  15,  16,  13,  14,
   4,   1,   2,   5,   7,
   4,   8,   6,   3,   4,
   4,   9,  12,  11,  10,
   4,  13,  16,  15,  14,
# unused index
 0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, MainWall-W    OPAQ  VERT  ext_wall     EXTERIOR       
  2, MainWall-N    OPAQ  VERT  ext_wall     EXTERIOR       
  3, MainWall-E    OPAQ  VERT  ext_wall     EXTERIOR       
  4, MainWall-S    OPAQ  VERT  ext_wall     EXTERIOR       
  5, to_bsm        OPAQ  FLOR  floors       Foundation     
  6, to_attic      OPAQ  CEIL  ceiling      attic          
  7, Door-S        OPAQ  VERT  ext_door     EXTERIOR       
  8, Window-S      TRAN  VERT  window       EXTERIOR       
# base
5   0   0   0   0   0   86.00    
