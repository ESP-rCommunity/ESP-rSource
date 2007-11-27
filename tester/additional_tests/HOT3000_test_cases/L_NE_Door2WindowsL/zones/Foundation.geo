# geometry of Foundation defined in: user/L_NE_Door2WindowsL/zones/Foundation.geo
GEN  Foundation  simplified basement elevation  # type, name, descr
      12        8 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  5.00000  0.00000   # vert 1  
    0.00000  10.00000  0.00000   # vert 2  
    0.00000  10.00000  3.00000   # vert 3  
    0.00000  5.00000  3.00000   # vert 4  
    10.00000  10.00000  0.00000   # vert 5  
    10.00000  10.00000  3.00000   # vert 6  
    10.00000  0.00000  0.00000   # vert 7  
    10.00000  0.00000  3.00000   # vert 8  
    5.00000  0.00000  0.00000   # vert 9  
    5.00000  0.00000  3.00000   # vert 10 
    5.00000  5.00000  0.00000   # vert 11 
    5.00000  5.00000  3.00000   # vert 12 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
   4,   2,   3,   6,   5,
   4,   5,   6,   8,   7,
   4,   7,   8,  10,   9,
   4,   9,  10,  12,  11,
   4,  11,  12,   4,   1,
   6,   1,   2,   5,   7,   9,  11,
   6,  12,  10,   8,   6,   3,   4,
# unused index
 0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, bsm_MainWall-W1  OPAQ  VERT  foundation   BASESIMP       
  2, bsm_MainWall-N  OPAQ  VERT  foundation   BASESIMP       
  3, bsm_MainWall-E  OPAQ  VERT  foundation   BASESIMP       
  4, bsm_MainWall-S1  OPAQ  VERT  foundation   BASESIMP       
  5, bsm_MainWall-W2  OPAQ  VERT  foundation   BASESIMP       
  6, bsm_MainWall-S2  OPAQ  VERT  foundation   BASESIMP       
  7, slab          OPAQ  FLOR  slab_floor   BASESIMP       
  8, to_main       OPAQ  CEIL  floors_r     main           
# base
7   0   0   0   0   0   75.00    
