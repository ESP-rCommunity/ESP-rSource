# geometry of bathrm defined in: ../zones/bathrm.geo
GEN  bathrm  bathrm represents the 1st level bathroom  # type, name, descr
      18      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.00000     7.00000     2.20000  # vert   1
      3.00000     7.00000     2.20000  # vert   2
      3.00000     5.00000     2.20000  # vert   3
      6.00000     5.00000     2.20000  # vert   4
      6.00000     7.00000     4.40000  # vert   5
      3.00000     7.00000     4.40000  # vert   6
      3.00000     5.00000     4.40000  # vert   7
      6.00000     5.00000     4.40000  # vert   8
      6.00000     5.50000     2.20000  # vert   9
      4.50000     5.50000     2.20000  # vert  10
      4.50000     7.00000     2.20000  # vert  11
      4.50000     5.00000     2.20000  # vert  12
      3.00000     5.50000     2.20000  # vert  13
      4.50000     5.00000     4.40000  # vert  14
      5.00000     7.00000     3.80000  # vert  15
      4.00000     7.00000     3.80000  # vert  16
      4.00000     7.00000     4.20000  # vert  17
      5.00000     7.00000     4.20000  # vert  18
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
   0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, north         OPAQ  VERT  ext_wall     EXTERIOR       
  2, bed1          OPAQ  VERT  int_part     bed1           
  3, w_heat_c      OPAQ  VERT  int_part     w_heat_c       
  4, east          OPAQ  VERT  ext_part     SIMILAR        
  5, loft          OPAQ  CEIL  loft_ceil    loft           
  6, porch         OPAQ  FLOR  upper_flor   EXTERIOR       
  7, bin_c         OPAQ  FLOR  upper_flor   bin_c          
  8, stairs        OPAQ  FLOR  upper_flor   stairs         
  9, hall_1        OPAQ  FLOR  upper_flor   hall_1         
 10, door_hall2    OPAQ  VERT  int_doors    hall_2         
 11, window        TRAN  VERT  dbl_glz      EXTERIOR       
# base
   6   7   8   9     6.00
