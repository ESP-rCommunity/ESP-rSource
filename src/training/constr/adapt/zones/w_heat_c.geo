# geometry of w_heat_c defined in: ../zones/w_heat_c.geo
GEN  w_heat_c  w_heat_c describes the boiler space  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.50000     5.00000     2.20000  # vert   1
      4.50000     4.00000     2.20000  # vert   2
      6.00000     4.00000     2.20000  # vert   3
      6.00000     5.00000     2.20000  # vert   4
      4.50000     5.00000     4.40000  # vert   5
      4.50000     4.00000     4.40000  # vert   6
      6.00000     4.00000     4.40000  # vert   7
      6.00000     5.00000     4.40000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
# number of default windows within each surface 
   0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, door_hall2    OPAQ  VERT  int_doors    hall_2         
  2, hall2         OPAQ  VERT  int_part     hall_2         
  3, east          OPAQ  VERT  ext_part     SIMILAR        
  4, bathrm        OPAQ  VERT  int_part     bathrm         
  5, loft          OPAQ  CEIL  loft_ceil    loft           
  6, stairs        OPAQ  FLOR  upper_flor   stairs         
# base
   6   0   0   0     1.50
