# geometry of w_heat_c defined in: w_heat_c.geo
GEN  w_heat_c               # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      1.500     2.000     2.200  # vert  1
      1.500     3.000     2.200  # vert  2
      0.000     3.000     2.200  # vert  3
      0.000     2.000     2.200  # vert  4
      1.500     2.000     4.400  # vert  5
      1.500     3.000     4.400  # vert  6
      0.000     3.000     4.400  # vert  7
      0.000     2.000     4.400  # vert  8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, hall2         OPAQ  VERT  int_doors    hall_2         
  2, hall2_2       OPAQ  VERT  int_part     hall_2         
  3, west_part     OPAQ  VERT  ext_part     SIMILAR        
  4, bathrm        OPAQ  VERT  int_part     bathrm         
  5, ceiling       OPAQ  CEIL  loft_ceil    loft           
  6, floor         OPAQ  FLOR  upper_flor   stairs         
