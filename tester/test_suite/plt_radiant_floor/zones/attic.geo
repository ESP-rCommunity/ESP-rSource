# geometry of attic defined in: ../zones/attic.geo
GEN  attic  attic zone  # type, name, descr
       9       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     2.70000  # vert   1
     10.00000     0.00000     2.70000  # vert   2
     10.00000     9.00000     2.70000  # vert   3
      5.00000     9.00000     2.70000  # vert   4
      5.00000    15.00000     2.70000  # vert   5
      0.00000    15.00000     2.70000  # vert   6
      5.00000     0.00000     5.03000  # vert   7
      5.00000    15.00000     5.03000  # vert   8
     10.00000    15.00000     2.70000  # vert   9
# no of vertices followed by list of associated vert
   3,  1,  2,  7,
   5,  2,  3,  9,  8,  7,
   4,  6,  8,  9,  5,
   4,  1,  7,  8,  6,
   6,  2,  1,  6,  5,  4,  3,
   4,  9,  3,  4,  5,
# unused index
  0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Southwall     OPAQ  VERT  roof         EXTERIOR       
  2, eastwall      OPAQ  SLOP  roof         EXTERIOR       
  3, northwall     OPAQ  VERT  roof         EXTERIOR       
  4, westwall      OPAQ  SLOP  roof         EXTERIOR       
  5, floor-main    OPAQ  FLOR  ceiling_inv  main           
  6, floor-garge   OPAQ  FLOR  ceiling_inv  garage         
# base
  5  6  0  0  0  0   150.00
