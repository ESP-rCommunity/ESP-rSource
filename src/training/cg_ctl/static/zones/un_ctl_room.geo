# geometry of un_ctl_room defined in: un_ctl_room.geo
GEN  un_ctl_room            # type   zone name
      12       7   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      5.00000     0.00000     0.00000  # vert  1
      9.00000     0.00000     0.00000  # vert  2
      9.00000     5.00000     0.00000  # vert  3
      5.00000     5.00000     0.00000  # vert  4
      5.00000     0.00000     3.00000  # vert  5
      9.00000     0.00000     3.00000  # vert  6
      9.00000     5.00000     3.00000  # vert  7
      5.00000     5.00000     3.00000  # vert  8
      5.80000     0.00000     0.90000  # vert  9
      8.20000     0.00000     0.90000  # vert 10
      8.20000     0.00000     2.70000  # vert 11
      5.80000     0.00000     2.70000  # vert 12
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
  1, Surf-1        OPAQ  VERT  ext_wall     EXTERIOR       
  2, Surf-2        OPAQ  VERT  ext_wall     EXTERIOR       
  3, Surf-3        OPAQ  VERT  ext_wall     EXTERIOR       
  4, Surf-4        OPAQ  VERT  ext_wall     EXTERIOR       
  5, Surf-5        OPAQ  CEIL  roof         EXTERIOR       
  6, Surf-6        OPAQ  FLOR  grnd_floor   EXTERIOR       
  7, window        TRAN  VERT  dbl_glz      EXTERIOR       
