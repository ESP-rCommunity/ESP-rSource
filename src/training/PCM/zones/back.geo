# geometry of back defined in: ../zones/back.geo
GEN  back  back describes another zone of the building  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     3.00000     0.00000  # vert   1
      5.00000     3.00000     0.00000  # vert   2
      5.00000     6.00000     0.00000  # vert   3
      0.00000     6.00000     0.00000  # vert   4
      0.00000     3.00000     2.70000  # vert   5
      5.00000     3.00000     2.70000  # vert   6
      5.00000     6.00000     2.70000  # vert   7
      0.00000     6.00000     2.70000  # vert   8
# no of vertices followed by list of associated vert
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  1,  2,  6,  5,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Wall-2        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Wall-3        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Wall-4        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Top-5         OPAQ  CEIL  roof         EXTERIOR       
  5, Base-6        OPAQ  FLOR  grnd_floor   GROUND         
  6, Without_PCM   OPAQ  VERT  partition    ANOTHER        
# base
  6  5  0  0  0  0    28.50 0
