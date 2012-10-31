# geometry of kitchen defined in: kitchen.geo
GEN  kitchen                # type   zone name
      16       9   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      3.000     0.000     0.000  # vert  1
      6.000     0.000     0.000  # vert  2
      6.000     4.000     0.000  # vert  3
      3.000     4.000     0.000  # vert  4
      3.000     0.000     2.200  # vert  5
      6.000     0.000     2.200  # vert  6
      6.000     4.000     2.200  # vert  7
      3.000     4.000     2.200  # vert  8
      3.000     3.000     2.200  # vert  9
      6.000     3.000     2.200  # vert 10
      3.000     1.500     2.200  # vert 11
      3.000     1.500     0.000  # vert 12
      4.000     0.000     0.800  # vert 13
      5.200     0.000     0.800  # vert 14
      5.200     0.000     2.000  # vert 15
      4.000     0.000     2.000  # vert 16
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1, 13, 16, 15, 14, 13,
   5,  2,  3,  7, 10,  6,
   4,  3,  4,  8,  7,
   5,  4, 12, 11,  9,  8,
   5,  5,  6, 10,  9, 11,
   5,  4,  3,  2,  1, 12,
   4,  9, 10,  7,  8,
   4, 12,  1,  5, 11,
   4, 13, 14, 15, 16,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  ext_wall     EXTERIOR       
  2, east_part     OPAQ  VERT  ext_part     SIMILAR        
  3, liv_r         OPAQ  VERT  int_part     liv_room       
  4, hall          OPAQ  VERT  int_part     hall_1         
  5, bed_1         OPAQ  CEIL  ceiling      bed_south      
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, north_beds    OPAQ  CEIL  ceiling      north_beds     
  8, Ext           OPAQ  VERT  ext_wall     EXTERIOR       
  9, window        TRAN  VERT  single_glaz  EXTERIOR       
