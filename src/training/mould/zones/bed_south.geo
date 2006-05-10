# geometry of bed_south defined in: bed_south.geo
GEN  bed_south              # type   zone name
      14       8   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      3.000     0.000     2.200  # vert  1
      6.000     0.000     2.200  # vert  2
      6.000     3.000     2.200  # vert  3
      3.000     3.000     2.200  # vert  4
      3.000     0.000     4.400  # vert  5
      6.000     0.000     4.400  # vert  6
      6.000     3.000     4.400  # vert  7
      3.000     3.000     4.400  # vert  8
      3.000     2.000     2.200  # vert  9
      3.000     2.000     4.400  # vert 10
      4.000     0.000     3.000  # vert 11
      5.200     0.000     3.000  # vert 12
      5.200     0.000     4.000  # vert 13
      4.000     0.000     4.000  # vert 14
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1, 11, 14, 13, 12, 11,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  9,  1,  5, 10,
   5,  5,  6,  7,  8, 10,
   5,  4,  3,  2,  1,  9,
   4,  4,  9, 10,  8,
   4, 11, 12, 13, 14,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  ext_wall     EXTERIOR       
  2, east          OPAQ  VERT  ext_part     SIMILAR        
  3, north_bed     OPAQ  VERT  int_part     north_beds     
  4, bathrm        OPAQ  VERT  int_part     bathrm         
  5, roof          OPAQ  CEIL  loft_ceil    loft           
  6, floor         OPAQ  FLOR  upper_flor   kitchen        
  7, hall_2        OPAQ  VERT  int_part     hall_2         
  8, window        TRAN  VERT  single_glaz  EXTERIOR       
