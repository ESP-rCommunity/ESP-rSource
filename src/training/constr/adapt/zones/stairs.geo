# geometry of stairs defined in: ../zones/stairs.geo
GEN  stairs  stairs describes a connecting zone  # type, name, descr
      12       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.00000     5.50000     0.00000  # vert   1
      4.50000     5.50000     0.00000  # vert   2
      4.50000     3.00000     0.00000  # vert   3
      6.00000     3.00000     0.00000  # vert   4
      6.00000     5.50000     2.20000  # vert   5
      4.50000     5.50000     2.20000  # vert   6
      4.50000     3.00000     2.20000  # vert   7
      6.00000     3.00000     2.20000  # vert   8
      6.00000     4.00000     2.20000  # vert   9
      6.00000     5.00000     2.20000  # vert  10
      4.50000     5.00000     2.20000  # vert  11
      4.50000     4.00000     2.20000  # vert  12
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
   0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, bin_c         OPAQ  VERT  ext_part     bin_c          
  2, hall1         OPAQ  VERT  int_doors    hall_1         
  3, livrm         OPAQ  VERT  int_part     liv_room       
  4, east          OPAQ  VERT  ext_part     SIMILAR        
  5, bathrm        OPAQ  CEIL  ceiling      bathrm         
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, w_heat_c      OPAQ  CEIL  ceiling      w_heat_c       
  8, hall2         OPAQ  CEIL  int_doors    hall_2         
# base
   6   0   0   0     3.75
