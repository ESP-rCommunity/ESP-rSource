# geometry of Foundation-1 defined in: user/obs_4_eachface/zones/Foundation-1.geo
GEN  Foundation-1  simplified basement elevation  # type, name, descr
       8        6 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    -1.00000  0.75000  -1.82000   # vert 1  
    -1.00000  0.75000  0.53000   # vert 2  
    -10.74600  0.75000  0.53000   # vert 3  
    -10.74600  0.75000  -1.82000   # vert 4  
    -10.74600  10.49600  0.53000   # vert 5  
    -10.74600  10.49600  -1.82000   # vert 6  
    -1.00000  10.49600  0.53000   # vert 7  
    -1.00000  10.49600  -1.82000   # vert 8  
# no of vertices followed by list of associated vert
   4,   1,   2,   3,   4,
   4,   4,   3,   5,   6,
   4,   6,   5,   7,   8,
   4,   8,   7,   2,   1,
   4,   1,   4,   6,   8,
   4,   7,   5,   3,   2,
# unused index
 0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, bsm_Mainflr2  OPAQ  VERT  foundation   BASESIMP       
  2, bsm_Mainflr1  OPAQ  VERT  foundation   BASESIMP       
  3, bsm_Mainflr4  OPAQ  VERT  foundation   BASESIMP       
  4, bsm_Mainflr3  OPAQ  VERT  foundation   BASESIMP       
  5, slab          OPAQ  FLOR  slab_floor   BASESIMP       
  6, to_main       OPAQ  CEIL  floors_r     Main           
# base
5   0   0   0   0   0   94.98    
