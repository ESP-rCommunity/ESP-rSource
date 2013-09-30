# geometry of room defined in: ../zones/room.geo
GEN  room  room describes a...  # type, name, descr
      12       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      5.00000     0.00000     0.00000  # vert   2
      5.00000     5.00000     0.00000  # vert   3
      0.00000     5.00000     0.00000  # vert   4
      0.00000     0.00000     3.00000  # vert   5
      5.00000     0.00000     3.00000  # vert   6
      5.00000     5.00000     3.00000  # vert   7
      0.00000     5.00000     3.00000  # vert   8
      1.00000     0.00000     0.00000  # vert   9
      1.00000     0.00000     3.00000  # vert  10
      0.00000     1.00000     0.00000  # vert  11
      0.00000     1.00000     3.00000  # vert  12
# no of vertices followed by list of associated vert
   4,  9,  2,  6, 10,
   4,  2,  3,  7,  6,
   4,  4, 11, 12,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  3,  4,  8,  7,
   4,  1,  9, 10,  5,
   4, 11,  1,  5, 12,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  intern_wall  EXTERIOR       
  2, east          OPAQ  VERT  intern_wall  EXTERIOR       
  3, west          OPAQ  VERT  intern_wall  EXTERIOR       
  4, roof          OPAQ  CEIL  roof_2       EXTERIOR       
  5, floor         OPAQ  FLOR  floor_1      GROUND         
  6, north_1       OPAQ  VERT  intern_wall  EXTERIOR       
  7, edge_s        OPAQ  VERT  intern_wall  EXTERIOR       
  8, edge_w        OPAQ  VERT  intern_wall  EXTERIOR       
# base
  5  0  0  0  0  0    25.00 0
