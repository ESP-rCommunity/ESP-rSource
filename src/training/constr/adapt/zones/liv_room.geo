# geometry of liv_room defined in: ../zones/liv_room.geo
GEN  liv_room  liv_room is a ground level room adjacent to kitchen  # type, name, descr
      18      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.00000     3.00000     0.00000  # vert   1
      0.00000     3.00000     0.00000  # vert   2
      0.00000     0.00000     0.00000  # vert   3
      6.00000     0.00000     0.00000  # vert   4
      6.00000     3.00000     2.20000  # vert   5
      0.00000     3.00000     2.20000  # vert   6
      0.00000     0.00000     2.20000  # vert   7
      6.00000     0.00000     2.20000  # vert   8
      4.50000     3.00000     2.20000  # vert   9
      4.50000     3.00000     0.00000  # vert  10
      3.00000     3.00000     0.00000  # vert  11
      3.00000     3.00000     2.20000  # vert  12
      0.50000     0.00000     0.80000  # vert  13
      2.50000     0.00000     0.80000  # vert  14
      2.50000     0.00000     2.00000  # vert  15
      0.50000     0.00000     2.00000  # vert  16
      3.00000     0.00000     2.20000  # vert  17
      3.00000     0.00000     0.00000  # vert  18
# no of vertices followed by list of associated vert
   4,  1, 10,  9,  5,
   4,  2,  3,  7,  6,
  10,  3, 18, 17,  7,  3, 13, 16, 15, 14, 13,
   4,  4,  1,  5,  8,
   4, 12,  6,  7, 17,
   7,  4, 18,  3,  2, 11, 10,  1,
   4, 10, 11, 12,  9,
   4, 11,  2,  6, 12,
   4, 13, 14, 15, 16,
   5,  5,  9, 12, 17,  8,
   4, 18,  4,  8, 17,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, stairs        OPAQ  VERT  int_part     stairs         
  2, west          OPAQ  VERT  ext_part     SIMILAR        
  3, south         OPAQ  VERT  ext_wall     EXTERIOR       
  4, east          OPAQ  VERT  ext_part     SIMILAR        
  5, bed2          OPAQ  CEIL  ceiling      bed2           
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, door_hall1    OPAQ  VERT  int_doors    hall_1         
  8, kitch         OPAQ  VERT  int_part     kitchen        
  9, window        TRAN  VERT  dbl_glz      EXTERIOR       
 10, bed3          OPAQ  CEIL  ceiling      bed3           
 11, TIM           OPAQ  VERT  ext_wall     EXTERIOR       
# base
   6   0   0   0    18.00
