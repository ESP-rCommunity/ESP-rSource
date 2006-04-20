# geometry of stairs defined in: stairs.geo
GEN  stairs                 # type   zone name
      12       8   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.000     1.500     0.000  # vert  1
      1.500     1.500     0.000  # vert  2
      1.500     4.000     0.000  # vert  3
      0.000     4.000     0.000  # vert  4
      0.000     1.500     2.200  # vert  5
      1.500     1.500     2.200  # vert  6
      1.500     4.000     2.200  # vert  7
      0.000     4.000     2.200  # vert  8
      0.000     3.000     2.200  # vert  9
      0.000     2.000     2.200  # vert 10
      1.500     2.000     2.200  # vert 11
      1.500     3.000     2.200  # vert 12
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   6,  2,  3,  7, 12, 11,  6,
   4,  3,  4,  8,  7,
   6,  4,  1,  5, 10,  9,  8,
   4,  5,  6, 11, 10,
   4,  4,  3,  2,  1,
   4, 10, 11, 12,  9,
   4,  9, 12,  7,  8,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, bin           OPAQ  VERT  ext_part     bin_c          
  2, hall          OPAQ  VERT  int_part     hall_1         
  3, livrm         OPAQ  VERT  int_part     liv_room       
  4, west          OPAQ  VERT  ext_part     SIMILAR        
  5, bathrm        OPAQ  CEIL  ceiling      bathrm         
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, whc           OPAQ  CEIL  ceiling      w_heat_c       
  8, north_beds    OPAQ  CEIL  ceiling      hall_2         
