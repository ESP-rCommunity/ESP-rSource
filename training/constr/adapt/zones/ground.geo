# geometry of ground defined in: ../zone/ground.geo
GEN  ground                 # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
     -5.000    -5.000    -3.000  # vert  1
     11.000    -5.000    -3.000  # vert  2
     11.000    12.000    -3.000  # vert  3
     -5.000    12.000    -3.000  # vert  4
     -5.000    -5.000     0.000  # vert  5
     11.000    -5.000     0.000  # vert  6
     11.000    12.000     0.000  # vert  7
     -5.000    12.000     0.000  # vert  8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  fict_grnd    EXTERIOR       
  2, east          OPAQ  VERT  fict_grnd    EXTERIOR       
  3, north         OPAQ  VERT  fict_grnd    EXTERIOR       
  4, west          OPAQ  VERT  fict_grnd    EXTERIOR       
  5, top           OPAQ  CEIL  fict_grnd    EXTERIOR       
  6, bottom        OPAQ  FLOR  fict_grnd    EXTERIOR       
