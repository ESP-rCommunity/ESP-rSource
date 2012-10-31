# geometry of collector defined in: ../zone/collector.geo
GEN  collector  collector describes a solar collector (air based)  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      1.50000     0.90000     5.51300  # vert   1
      4.00000     0.90000     5.51300  # vert   2
      4.00000     2.74000     6.29700  # vert   3
      1.50000     2.74000     6.29700  # vert   4
      1.50000     0.91990     5.46712  # vert   5
      4.00000     0.91990     5.46712  # vert   6
      4.00000     2.75976     6.25129  # vert   7
      1.50000     2.75976     6.25129  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  3,  4,
   4,  6,  5,  8,  7,
   4,  2,  6,  7,  3,
   4,  3,  7,  8,  4,
   4,  1,  4,  8,  5,
   4,  2,  1,  5,  6,
# unused index
  0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, first         OPAQ  SLOP  roof         EXTERIOR       
  2, xxcollector   OPAQ  SLOP  collector    loft           
  3, xcoll-e       OPAQ  VERT  door         loft           
  4, xcoll-up      OPAQ  SLOP  door         loft           
  5, xcoll-w       OPAQ  VERT  door         loft           
  6, xcoll-dn      OPAQ  SLOP  door         loft           
# base
  0  0  0  0  0  0     0.00
