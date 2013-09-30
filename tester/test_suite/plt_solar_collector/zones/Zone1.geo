# geometry of Zone1 defined in: ../zones/Zone1.geo
GEN  Zone1  Zone1 describes a dummy zone  # type, name, descr
      10       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      2.00000     0.00000     0.00000  # vert   2
      2.00000     3.00000     0.00000  # vert   3
      0.00000     3.00000     0.00000  # vert   4
      0.00000     1.41400     3.00000  # vert   5
      2.00000     1.41400     3.00000  # vert   6
      2.00000     3.00000     3.00000  # vert   7
      0.00000     3.00000     3.00000  # vert   8
      0.00000     0.00000     1.58600  # vert   9
      2.00000     0.00000     1.58600  # vert  10
# no of vertices followed by list of associated vert
   4,  9, 10,  6,  5,
   5,  2,  3,  7,  6, 10,
   4,  3,  4,  8,  7,
   5,  4,  1,  9,  5,  8,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
   4,  1,  2, 10,  9,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Pyrano        OPAQ  SLOP  Pyranometer  EXTERIOR       
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof         EXTERIOR       
  6, Surf-6        OPAQ  FLOR  grnd_floor   GROUND         
  7, Surf-7        OPAQ  VERT  extern_wall  EXTERIOR       
# base
  6  0  0  0  0  0     6.00 0
