# geometry of basement defined in: ../zones/basement.geo
GEN  basement  simplified basement elevation  # type, name, descr
      12       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
     12.00000     0.00000     0.00000  # vert   2
      0.00000    12.20000     0.00000  # vert   3
      0.00000     0.00000     2.60000  # vert   4
     12.00000     0.00000     2.60000  # vert   5
      0.00000    12.20000     2.60000  # vert   6
      4.97000    12.20000     2.60000  # vert   7
      4.97000     6.79000     2.60000  # vert   8
     12.00000     6.79000     2.60000  # vert   9
      4.97000     6.79000     0.00000  # vert  10
     12.00000     6.79000     0.00000  # vert  11
      4.97000    12.20000     0.00000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  5,  4,
   4,  2, 11,  9,  5,
   4,  3,  6,  7, 12,
   4,  3,  1,  4,  6,
   6,  4,  5,  9,  8,  7,  6,
   6,  1,  3, 12, 10, 11,  2,
   4, 12,  7,  8, 10,
   4, 11, 10,  8,  9,
# unused index
  0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  foundation_  BASESIMP       
  2, Surf-2        OPAQ  VERT  foundation_  BASESIMP       
  3, Surf-5        OPAQ  VERT  foundation_  BASESIMP       
  4, Surf-6        OPAQ  VERT  foundation_  BASESIMP       
  5, to_upstairs   OPAQ  CEIL  floors_r     main_second    
  6, floor_slab    OPAQ  FLOR  slab_floor   BASESIMP       
  7, Surf-7        OPAQ  VERT  foundation_  BASESIMP       
  8, Surf-8        OPAQ  VERT  foundation_  BASESIMP       
# base
   6   0   0   0   0   0   108.37
