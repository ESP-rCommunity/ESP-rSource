# geometry of hall_1 defined in: hall_1.geo
GEN  hall_1                 # type   zone name
      10       7   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      1.500     1.500     0.000  # vert  1
      3.000     1.500     0.000  # vert  2
      3.000     4.000     0.000  # vert  3
      1.500     4.000     0.000  # vert  4
      1.500     1.500     2.200  # vert  5
      3.000     1.500     2.200  # vert  6
      3.000     4.000     2.200  # vert  7
      1.500     4.000     2.200  # vert  8
      1.500     2.000     2.200  # vert  9
      3.000     2.000     2.200  # vert 10
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   5,  2,  3,  7, 10,  6,
   4,  3,  4,  8,  7,
   5,  4,  1,  5,  9,  8,
   4,  9, 10,  7,  8,
   4,  4,  3,  2,  1,
   4,  5,  6, 10,  9,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, door          OPAQ  VERT  int_doors    EXTERIOR       
  2, kitchen       OPAQ  VERT  int_part     kitchen        
  3, livroom       OPAQ  VERT  int_doors    liv_room       
  4, stairs        OPAQ  VERT  int_part     stairs         
  5, hall_2        OPAQ  CEIL  ceiling      hall_2         
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, bathrm        OPAQ  CEIL  ceiling      bathrm         
