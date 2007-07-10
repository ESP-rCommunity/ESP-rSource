# geometry of Foundation defined in: user/T_W_Door2WindowsL/zones/Foundation.geo
GEN  Foundation  simplified basement elevation  # type, name, descr
      20       11 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  0.00000   # vert 1  
    0.00000  10.00000  0.00000   # vert 2  
    0.00000  10.00000  2.44000   # vert 3  
    0.00000  0.00000  2.44000   # vert 4  
    5.00000  10.00000  0.00000   # vert 5  
    5.00000  10.00000  2.44000   # vert 6  
    5.00000  7.00000  0.00000   # vert 7  
    5.00000  7.00000  2.44000   # vert 8  
    9.00000  7.00000  0.00000   # vert 9  
    9.00000  7.00000  2.44000   # vert 10 
    9.00000  2.00000  0.00000   # vert 11 
    9.00000  2.00000  2.44000   # vert 12 
    4.00000  2.00000  0.00000   # vert 13 
    4.00000  2.00000  2.44000   # vert 14 
    4.00000  0.00000  0.00000   # vert 15 
    4.00000  0.00000  2.44000   # vert 16 
    8.99000  2.00000  1.93000   # vert 17 
    8.49000  2.00000  1.93000   # vert 18 
    8.49000  2.00000  2.43000   # vert 19 
    8.99000  2.00000  2.43000   # vert 20 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
   4,   2,   3,   6,   5,
   4,   5,   6,   8,   7,
   4,   7,   8,  10,   9,
   4,   9,  10,  12,  11,
  10,  11,  12,  14,  13,  11,  18,  19,  20,  17,  18,
   4,  13,  14,  16,  15,
   4,  15,  16,   4,   1,
   8,   1,   2,   5,   7,   9,  11,  13,  15,
   8,  16,  14,  12,  10,   8,   6,   3,   4,
   4,  17,  20,  19,  18,
# unused index
 0   0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, bsm_MainWall-W  OPAQ  VERT  foundation   BASESIMP       
  2, bsm_MainWall-N1  OPAQ  VERT  foundation   BASESIMP       
  3, bsm_MainWall-E1  OPAQ  VERT  foundation   BASESIMP       
  4, bsm_MainWall-N2  OPAQ  VERT  foundation   BASESIMP       
  5, bsm_MainWall-E2  OPAQ  VERT  foundation   BASESIMP       
  6, bsm_MainWall-S1  OPAQ  VERT  foundation   BASESIMP       
  7, bsm_MainWall-E3  OPAQ  VERT  foundation   BASESIMP       
  8, bsm_MainWall-S2  OPAQ  VERT  foundation   BASESIMP       
  9, slab          OPAQ  FLOR  slab_floor   BASESIMP       
 10, to_main       OPAQ  CEIL  floors_r     main           
 11, Window-27_b   TRAN  VERT  window       EXTERIOR       
# base
9   0   0   0   0   0   50.00    
