# geometry of mixing_box defined in: ../zones/mixing_box.geo
GEN  mixing_box  mixing_box is a zonal representation of ahu  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     6.00000     3.00000  # vert   1
      3.00000     6.00000     3.00000  # vert   2
      3.00000    12.00000     3.00000  # vert   3
      0.00000    12.00000     3.00000  # vert   4
      0.00000     6.00000     3.50000  # vert   5
      3.00000     6.00000     3.50000  # vert   6
      3.00000    12.00000     3.50000  # vert   7
      0.00000    12.00000     3.50000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# unused index
  0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  gyp_blk_ptn  ceil_void      
  2, Surf-2        OPAQ  VERT  gyp_blk_ptn  ADIABATIC      
  3, Surf-3        OPAQ  VERT  gyp_blk_ptn  ADIABATIC      
  4, Surf-4        OPAQ  VERT  gyp_blk_ptn  ADIABATIC      
  5, Surf-5        OPAQ  CEIL  gyp_blk_ptn  ADIABATIC      
  6, Surf-6        OPAQ  FLOR  gyp_blk_ptn  ADIABATIC      
# base
  6  0  0  0  0  0    18.00
