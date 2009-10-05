# geometry of hall_1 defined in: ../zones/hall_1.geo
GEN  hall_1  hall_1 is a space adjacent to the ground level stairs  # type, name, descr
      12       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.50000     5.50000     0.00000  # vert   1
      3.00000     5.50000     0.00000  # vert   2
      3.00000     3.00000     0.00000  # vert   3
      4.50000     3.00000     0.00000  # vert   4
      4.50000     5.50000     2.20000  # vert   5
      3.00000     5.50000     2.20000  # vert   6
      3.00000     3.00000     2.20000  # vert   7
      4.50000     3.00000     2.20000  # vert   8
      4.50000     5.00000     2.20000  # vert   9
      3.00000     5.00000     2.20000  # vert  10
      3.00000     4.00000     0.00000  # vert  11
      3.00000     4.00000     2.20000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   5,  2, 11, 12, 10,  6,
   4,  3,  4,  8,  7,
   5,  4,  1,  5,  9,  8,
   5,  9, 10, 12,  7,  8,
   5,  4,  3, 11,  2,  1,
   4,  5,  6, 10,  9,
   4, 11,  3,  7, 12,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, door_ext      OPAQ  VERT  int_doors    EXTERIOR       
  2, kitchn        OPAQ  VERT  int_part     kitchen        
  3, door_living   OPAQ  VERT  int_doors    liv_room       
  4, stairs        OPAQ  VERT  int_doors    stairs         
  5, hall_2        OPAQ  CEIL  ceiling      hall_2         
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, bathrm        OPAQ  CEIL  ceiling      bathrm         
  8, door_kitchn   OPAQ  VERT  int_doors    kitchen        
# base
   6   0   0   0     3.75
