# geometry of east defined in: ../zones/east.geo
GEN  east  south facing perimeter zone  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.00000     0.00000     0.00000  # vert   1
     10.00000    30.00000     0.00000  # vert   2
      5.00000    30.00000     0.00000  # vert   3
      5.00000     0.00000     0.00000  # vert   4
     10.00000     0.00000     2.70000  # vert   5
     10.00000    30.00000     2.70000  # vert   6
      5.00000    30.00000     2.70000  # vert   7
      5.00000     0.00000     2.70000  # vert   8
     10.00000     1.00000     1.00000  # vert   9
     10.00000    29.00000     1.00000  # vert  10
     10.00000    29.00000     2.00000  # vert  11
     10.00000     1.00000     2.00000  # vert  12
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
# number of default windows within each surface 
   0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, east          OPAQ  VERT  wall_NECB    EXTERIOR       
  2, north         OPAQ  VERT  wall_NECB    EXTERIOR       
  3, internal      OPAQ  VERT  partition    south          
  4, south         OPAQ  VERT  wall_NECB    EXTERIOR       
  5, ceiling       OPAQ  CEIL  roof_NECB    EXTERIOR       
  6, floor         OPAQ  FLOR  conc_deck    ADIABATIC      
  7, glazing       TRAN  VERT  window_NECB  EXTERIOR       
# base
   6   0   0   0   150.00
