# geometry of basement defined in: user/SimpleSqHseL/zones/basement.geo
GEN  basement  simplified basement elevation  # type, name, descr
      11        6 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  0.00000   # vert 1  
    0.00000  6.00000  0.00000   # vert 2  
    0.00000  6.00000  2.60000   # vert 3  
    0.00000  0.00000  2.60000   # vert 4  
    6.00000  6.00000  0.00000   # vert 5  
    6.00000  6.00000  2.60000   # vert 6  
    6.00000  0.00000  0.00000   # vert 7  
    6.00000  0.00000  2.60000   # vert 8  
    1.24900  0.00000  2.60000   # vert 9  
    1.24900  1.24900  2.60000   # vert 10 
    0.00000  1.24900  2.60000   # vert 11 
# no of vertices followed by list of associated vert
   4,   1,   4,   3,   2,
   4,   2,   3,   6,   5,
   4,   5,   6,   8,   7,
   4,   7,   8,   4,   1,
   4,   1,   2,   5,   7,
   8,   8,   6,   3,   4,   9,  10,  11,   4,
# unused index
 0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, bsm_Wall-W    OPAQ  VERT  foundation   BASESIMP       
  2, bsm_Wall-N    OPAQ  VERT  foundation   BASESIMP       
  3, bsm_Wall-E    OPAQ  VERT  foundation   BASESIMP       
  4, bsm_Wall-S    OPAQ  VERT  foundation   BASESIMP       
  5, slab          OPAQ  FLOR  slab_floor   BASESIMP       
  6, to_main       OPAQ  CEIL  floors_r     main           
# base
5   0   0   0   0   0   36.00    
