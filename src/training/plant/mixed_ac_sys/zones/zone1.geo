# geometry of zone1 defined in: zone1.geo
GEN  zone1                  # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.000     0.000     0.000  # vert  1
      0.000     0.000     0.000  # vert  2
      0.000     0.000     0.000  # vert  3
      0.000     0.000     0.000  # vert  4
      0.000     0.000     2.400  # vert  5
      0.000     0.000     2.400  # vert  6
      0.000     0.000     2.400  # vert  7
      0.000     0.000     2.400  # vert  8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface
   0,  0,  0,  0,  0,  0,
# number of default doors within each surface
   0,  0,  0,  0,  0,  0,
    3   0   0   0    # default insolation distribution
# surface attributes follow:
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  UNKN         UNKNOWN
  2, Surf-2        OPAQ  VERT  UNKN         UNKNOWN
  3, Surf-3        OPAQ  VERT  UNKN         UNKNOWN
  4, Surf-4        OPAQ  VERT  UNKN         UNKNOWN
  5, Surf-5        OPAQ  VERT  UNKN         UNKNOWN
  6, Surf-6        OPAQ  VERT  UNKN         UNKNOWN
