# geometry of hall_2 defined in: hall_2.geo
GEN  hall_2                 # type   zone name
      15      10   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      1.500     2.000     2.200  # vert  1
      3.000     2.000     2.200  # vert  2
      3.000     4.000     2.200  # vert  3
      0.000     4.000     2.200  # vert  4
      0.000     3.000     2.200  # vert  5
      1.500     3.000     2.200  # vert  6
      1.500     2.000     4.400  # vert  7
      3.000     2.000     4.400  # vert  8
      3.000     4.000     4.400  # vert  9
      0.000     4.000     4.400  # vert 10
      0.000     3.000     4.400  # vert 11
      1.500     3.000     4.400  # vert 12
      3.000     3.000     2.200  # vert 13
      3.000     3.000     4.400  # vert 14
      1.500     4.000     2.200  # vert 15
# no of vertices followed by list of associated vert
   4,  1,  2,  8,  7,
   4,  2, 13, 14,  8,
   5,  3, 15,  4, 10,  9,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
   4,  6,  1,  7, 12,
   7,  7,  8, 14,  9, 10, 11, 12,
   6,  6, 15,  3, 13,  2,  1,
   4, 13,  3,  9, 14,
   4,  6,  5,  4, 15,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   8   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, bathrm        OPAQ  VERT  int_doors    bathrm         
  2, south_beds    OPAQ  VERT  int_part     bed_south      
  3, north_beds    OPAQ  VERT  int_part     north_beds     
  4, west          OPAQ  VERT  ext_part     SIMILAR        
  5, whc           OPAQ  VERT  int_part     w_heat_c       
  6, whc2          OPAQ  VERT  int_doors    w_heat_c       
  7, ceiling       OPAQ  CEIL  loft_ceil    loft           
  8, hall1         OPAQ  FLOR  upper_flor   hall_1         
  9, north_beds    OPAQ  VERT  int_part     north_beds     
 10, stairs        OPAQ  FLOR  upper_flor   stairs         
