# geometry of bathrm defined in: bathrm.geo
GEN  bathrm                 # type   zone name
      18      11   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.000     0.000     2.200  # vert  1
      3.000     0.000     2.200  # vert  2
      3.000     2.000     2.200  # vert  3
      0.000     2.000     2.200  # vert  4
      0.000     0.000     4.400  # vert  5
      3.000     0.000     4.400  # vert  6
      3.000     2.000     4.400  # vert  7
      0.000     2.000     4.400  # vert  8
      0.000     1.500     2.200  # vert  9
      1.500     1.500     2.200  # vert 10
      1.500     0.000     2.200  # vert 11
      1.500     2.000     2.200  # vert 12
      3.000     1.500     2.200  # vert 13
      1.500     2.000     4.400  # vert 14
      1.000     0.000     3.800  # vert 15
      2.000     0.000     3.800  # vert 16
      2.000     0.000     4.200  # vert 17
      1.000     0.000     4.200  # vert 18
# no of vertices followed by list of associated vert
  11,  1, 11,  2,  6,  5,  1, 15, 18, 17, 16, 15,
   5,  2, 13,  3,  7,  6,
   4, 12,  4,  8, 14,
   5,  4,  9,  1,  5,  8,
   5,  5,  6,  7, 14,  8,
   4, 13,  2, 11, 10,
   4,  1,  9, 10, 11,
   4,  9,  4, 12, 10,
   4,  3, 13, 10, 12,
   4,  3, 12, 14,  7,
   4, 15, 16, 17, 18,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south_ext     OPAQ  VERT  ext_wall     EXTERIOR       
  2, s_bed         OPAQ  VERT  int_part     bed_south      
  3, wh_cup        OPAQ  VERT  int_part     w_heat_c       
  4, west          OPAQ  VERT  ext_part     SIMILAR        
  5, roof          OPAQ  CEIL  loft_ceil    loft           
  6, porch         OPAQ  FLOR  upper_flor   EXTERIOR       
  7, bin_c         OPAQ  FLOR  upper_flor   bin_c          
  8, stairs        OPAQ  FLOR  upper_flor   stairs         
  9, hall_1        OPAQ  FLOR  upper_flor   hall_1         
 10, hall_2        OPAQ  VERT  int_doors    hall_2         
 11, window        TRAN  VERT  single_glaz  EXTERIOR       
