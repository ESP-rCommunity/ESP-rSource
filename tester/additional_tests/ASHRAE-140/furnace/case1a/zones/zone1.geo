# geometry of zone1 defined in: ../zones/zone1.geo
GEN  zone1  zone1 describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      8.00000     0.00000     0.00000  # vert   2
      8.00000     6.00000     0.00000  # vert   3
      0.00000     6.00000     0.00000  # vert   4
      0.00000     0.00000     2.70000  # vert   5
      8.00000     0.00000     2.70000  # vert   6
      8.00000     6.00000     2.70000  # vert   7
      0.00000     6.00000     2.70000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, wall-1        OPAQ  VERT  wall         EXTERIOR       
  2, wall-2        OPAQ  VERT  wall         EXTERIOR       
  3, wall-3        OPAQ  VERT  wall         EXTERIOR       
  4, wall-4        OPAQ  VERT  wall         EXTERIOR       
  5, roof          OPAQ  CEIL  roof         EXTERIOR       
  6, floor         OPAQ  FLOR  floor        EXTERIOR       
# base
  6  0  0  0  0  0    48.00 0
