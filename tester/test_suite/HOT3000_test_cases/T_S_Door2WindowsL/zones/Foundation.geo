# geometry of Foundation defined in: user/T_S_Door2WindowsL/zones/Foundation.geo
GEN  Foundation  simplified basement elevation  # type, name, descr
      16       10 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  0.00000   # vert 1  
    0.00000  5.00000  0.00000   # vert 2  
    0.00000  5.00000  2.44000   # vert 3  
    0.00000  0.00000  2.44000   # vert 4  
    3.00000  5.00000  0.00000   # vert 5  
    3.00000  5.00000  2.44000   # vert 6  
    3.00000  9.00000  0.00000   # vert 7  
    3.00000  9.00000  2.44000   # vert 8  
    8.00000  9.00000  0.00000   # vert 9  
    8.00000  9.00000  2.44000   # vert 10 
    8.00000  4.00000  0.00000   # vert 11 
    8.00000  4.00000  2.44000   # vert 12 
    10.00000  4.00000  0.00000   # vert 13 
    10.00000  4.00000  2.44000   # vert 14 
    10.00000  0.00000  0.00000   # vert 15 
    10.00000  0.00000  2.44000   # vert 16 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
   4,   2,   3,   6,   5,
   4,   5,   6,   8,   7,
   4,   7,   8,  10,   9,
   4,   9,  10,  12,  11,
   4,  11,  12,  14,  13,
   4,  13,  14,  16,  15,
   4,  15,  16,   4,   1,
   8,   1,   2,   5,   7,   9,  11,  13,  15,
   8,  16,  14,  12,  10,   8,   6,   3,   4,
# unused index
 0   0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, bsm_MainWall-W1  OPAQ  VERT  foundation   BASESIMP       
  2, bsm_MainWall-N1  OPAQ  VERT  foundation   BASESIMP       
  3, bsm_MainWall-W2  OPAQ  VERT  foundation   BASESIMP       
  4, bsm_MainWall-N2  OPAQ  VERT  foundation   BASESIMP       
  5, bsm_MainWall-E1  OPAQ  VERT  foundation   BASESIMP       
  6, bsm_MainWall-N3  OPAQ  VERT  foundation   BASESIMP       
  7, bsm_MainWall-E2  OPAQ  VERT  foundation   BASESIMP       
  8, bsm_MainWall-S  OPAQ  VERT  foundation   BASESIMP       
  9, slab          OPAQ  FLOR  slab_floor   BASESIMP       
 10, to_main       OPAQ  CEIL  floors_r     main           
# base
9   0   0   0   0   0   70.00    
