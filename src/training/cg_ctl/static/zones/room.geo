# geometry of room defined in: room.geo
GEN  room                   # type   zone name
      12       7   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.000     0.000     0.000  # vert  1
      4.000     0.000     0.000  # vert  2
      4.000     5.000     0.000  # vert  3
      0.000     5.000     0.000  # vert  4
      0.000     0.000     3.000  # vert  5
      4.000     0.000     3.000  # vert  6
      4.000     5.000     3.000  # vert  7
      0.000     5.000     3.000  # vert  8
      0.800     0.000     0.900  # vert  9
      3.200     0.000     0.900  # vert 10
      3.200     0.000     2.700  # vert 11
      0.800     0.000     2.700  # vert 12
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  ext_wall     EXTERIOR       
  2, Surf-2        OPAQ  VERT  ext_wall     EXTERIOR       
  3, Surf-3        OPAQ  VERT  ext_wall     EXTERIOR       
  4, Surf-4        OPAQ  VERT  ext_wall     EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof         EXTERIOR       
  6, Surf-6        OPAQ  FLOR  grnd_floor   EXTERIOR       
  7, window        TRAN  VERT  dbl_glz      EXTERIOR       
