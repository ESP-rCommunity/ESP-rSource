# geometry of liv_room defined in: ../zone/liv_room.geo
GEN  liv_room               # type   zone name
      16       9   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.000     4.000     0.000  # vert  1
      6.000     4.000     0.000  # vert  2
      6.000     7.000     0.000  # vert  3
      0.000     7.000     0.000  # vert  4
      0.000     4.000     2.200  # vert  5
      6.000     4.000     2.200  # vert  6
      6.000     7.000     2.200  # vert  7
      0.000     7.000     2.200  # vert  8
      1.500     4.000     2.200  # vert  9
      1.500     4.000     0.000  # vert 10
      3.000     4.000     0.000  # vert 11
      3.000     4.000     2.200  # vert 12
      5.000     7.000     0.800  # vert 13
      3.000     7.000     0.800  # vert 14
      3.000     7.000     2.000  # vert 15
      5.000     7.000     2.000  # vert 16
# no of vertices followed by list of associated vert
   4,  1, 10,  9,  5,
   4,  2,  3,  7,  6,
  10,  3,  4,  8,  7,  3, 13, 16, 15, 14, 13,
   4,  4,  1,  5,  8,
   6,  5,  9, 12,  6,  7,  8,
   6,  4,  3,  2, 11, 10,  1,
   4, 10, 11, 12,  9,
   4, 11,  2,  6, 12,
   4, 13, 14, 15, 16,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, stairs        OPAQ  VERT  int_part     stairs         
  2, east_ext      OPAQ  VERT  ext_part     SIMILAR        
  3, north_ext     OPAQ  VERT  ext_wall     EXTERIOR       
  4, west_part     OPAQ  VERT  ext_part     SIMILAR        
  5, north_beds    OPAQ  CEIL  ceiling      north_beds     
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, hall_1        OPAQ  VERT  int_doors    hall_1         
  8, kitch         OPAQ  VERT  int_part     kitchen        
  9, window        TRAN  VERT  single_glaz  EXTERIOR       
