# geometry of loft defined in: loft.geo
GEN  loft                   # type   zone name
      17       9   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.000     0.000     4.400  # vert  1
      6.000     0.000     4.400  # vert  2
      6.000     7.000     4.400  # vert  3
      0.000     7.000     4.400  # vert  4
      0.000     3.500     6.200  # vert  5
      6.000     3.500     6.200  # vert  6
      0.000     2.000     4.400  # vert  7
      3.000     2.000     4.400  # vert  8
      3.000     0.000     4.400  # vert  9
      6.000     3.000     4.400  # vert 10
      3.000     3.000     4.400  # vert 11
      3.000     7.000     4.400  # vert 12
      0.000     4.000     4.400  # vert 13
      3.000     4.000     4.400  # vert 14
      0.000     3.000     4.400  # vert 15
      1.500     3.000     4.400  # vert 16
      1.500     2.000     4.400  # vert 17
# no of vertices followed by list of associated vert
   5,  7, 17,  8,  9,  1,
   6,  4, 13, 15,  7,  1,  5,
   4,  3,  6,  2, 10,
   5,  1,  9,  2,  6,  5,
   5,  4,  5,  6,  3, 12,
   7, 13,  4, 12,  3, 10, 11, 14,
   5,  9,  8, 11, 10,  2,
   7, 13, 14, 11,  8, 17, 16, 15,
   4,  7, 15, 16, 17,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, first         OPAQ  FLOR  loft_floor   bathrm         
  2, West_ext      OPAQ  VERT  ext_part     SIMILAR        
  3, East_ext      OPAQ  VERT  ext_part     SIMILAR        
  4, South_roof    OPAQ  UNKN  roof         EXTERIOR       
  5, North_roof    OPAQ  VERT  roof         EXTERIOR       
  6, Loft_nbed     OPAQ  FLOR  loft_floor   north_beds     
  7, Loft_sbed     OPAQ  FLOR  loft_floor   bed_south      
  8, hall          OPAQ  FLOR  loft_floor   hall_2         
  9, wh_cup        OPAQ  FLOR  loft_floor   w_heat_c       
