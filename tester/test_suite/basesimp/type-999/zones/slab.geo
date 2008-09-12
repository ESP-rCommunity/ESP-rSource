# geometry of slab defined in: ../zones/slab.geo
GEN  slab  slab describes a slab on grade foundation  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
     12.00000     0.00000     0.00000  # vert   2
     12.00000    12.00000     0.00000  # vert   3
      0.00000    12.00000     0.00000  # vert   4
      0.00000     0.00000     2.70000  # vert   5
     12.00000     0.00000     2.70000  # vert   6
     12.00000    12.00000     2.70000  # vert   7
      0.00000    12.00000     2.70000  # vert   8
# no of vertices followed by list of associated vert
   4,  4,  3,  2,  1,
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  1,  5,  8,  4,
   4,  5,  6,  7,  8,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, first         OPAQ  FLOR  EIA_Slab     BASESIMP       
  2, Surf-2        OPAQ  VERT  EIA_Wall     ADIABATIC      
  3, Surf-3        OPAQ  VERT  EIA_Wall     ADIABATIC      
  4, Surf-4        OPAQ  VERT  EIA_Wall     ADIABATIC      
  5, Surf-5        OPAQ  VERT  EIA_Wall     ADIABATIC      
  6, Surf-6        OPAQ  CEIL  EIA_Wall     ADIABATIC      
# base
  0  0  0  0  0  0   144.00
