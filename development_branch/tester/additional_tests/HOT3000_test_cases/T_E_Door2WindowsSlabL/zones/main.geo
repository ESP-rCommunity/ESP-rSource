# geometry of main defined in: user/T_E_Door2WindowsSlabL/zones/main.geo
GEN  main  simplified elevation of main zone  # type, name, descr
      27       13 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  2.00000  1.00000   # vert 1  
    0.00000  5.00000  1.00000   # vert 2  
    0.00000  5.00000  3.44000   # vert 3  
    0.00000  2.00000  3.44000   # vert 4  
    5.00000  5.00000  1.00000   # vert 5  
    5.00000  5.00000  3.44000   # vert 6  
    5.00000  10.00000  1.00000   # vert 7  
    5.00000  10.00000  3.44000   # vert 8  
    9.00000  10.00000  1.00000   # vert 9  
    9.00000  10.00000  3.44000   # vert 10 
    9.00000  0.00000  1.00000   # vert 11 
    9.00000  0.00000  3.44000   # vert 12 
    4.00000  0.00000  1.00000   # vert 13 
    4.00000  0.00000  3.44000   # vert 14 
    4.00000  2.00000  1.00000   # vert 15 
    4.00000  2.00000  3.44000   # vert 16 
    9.00000  9.99000  1.01000   # vert 17 
    9.00000  8.99000  1.01000   # vert 18 
    9.00000  8.99000  3.01000   # vert 19 
    9.00000  9.99000  3.01000   # vert 20 
    9.00000  7.99000  2.00000   # vert 21 
    9.00000  6.99000  2.00000   # vert 22 
    9.00000  6.99000  3.00000   # vert 23 
    9.00000  7.99000  3.00000   # vert 24 
    9.00000  6.99000  2.50000   # vert 25 
    9.00000  6.49000  2.50000   # vert 26 
    9.00000  6.49000  3.00000   # vert 27 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
   4,   2,   3,   6,   5,
   4,   5,   6,   8,   7,
   4,   7,   8,  10,   9,
  22,   9,  10,  12,  11,   9,  18,  19,  20,  17,  18,   9,  22,  23,  24,  21,  22,   9,  26,  27,  23,  25,  26,
   4,  11,  12,  14,  13,
   4,  13,  14,  16,  15,
   4,  15,  16,   4,   1,
   8,   1,   2,   5,   7,   9,  11,  13,  15,
   8,  16,  14,  12,  10,   8,   6,   3,   4,
   4,  17,  20,  19,  18,
   4,  21,  24,  23,  22,
   4,  25,  23,  27,  26,
# unused index
 0   0   0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, MainWall-03   OPAQ  VERT  ext_wall     EXTERIOR       
  2, MainWall-06   OPAQ  VERT  ext_wall     EXTERIOR       
  3, MainWall-05   OPAQ  VERT  ext_wall     EXTERIOR       
  4, MainWall-08   OPAQ  VERT  ext_wall     EXTERIOR       
  5, MainWall-01   OPAQ  VERT  ext_wall     EXTERIOR       
  6, MainWall-02   OPAQ  VERT  ext_wall     EXTERIOR       
  7, MainWall-07   OPAQ  VERT  ext_wall     EXTERIOR       
  8, MainWall-04   OPAQ  VERT  ext_wall     EXTERIOR       
  9, to_bsm        OPAQ  FLOR  slab_floor   BASESIMP       
 10, to_attic      OPAQ  CEIL  ceiling      attic          
 11, Door-3        OPAQ  VERT  ext_door     EXTERIOR       
 12, Window-25     TRAN  VERT  window       EXTERIOR       
 13, Window-26     TRAN  VERT  window       EXTERIOR       
# base
9   0   0   0   0   0   52.00    
