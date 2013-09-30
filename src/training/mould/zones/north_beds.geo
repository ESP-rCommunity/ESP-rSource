# geometry of north_beds defined in: north_beds.geo
GEN  north_beds             # type   zone name
      27      14   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.000     4.000     2.200  # vert  1
      3.000     4.000     2.200  # vert  2
      3.000     3.000     2.200  # vert  3
      6.000     3.000     2.200  # vert  4
      6.000     7.000     2.200  # vert  5
      3.000     7.000     2.200  # vert  6
      3.000     4.500     2.200  # vert  7
      3.000     7.000     2.200  # vert  8
      0.000     7.000     2.200  # vert  9
      0.000     4.000     4.400  # vert 10
      3.000     4.000     4.400  # vert 11
      3.000     3.000     4.400  # vert 12
      6.000     3.000     4.400  # vert 13
      6.000     7.000     4.400  # vert 14
      3.000     7.000     4.400  # vert 15
      3.000     4.500     4.400  # vert 16
      3.000     7.000     4.400  # vert 17
      0.000     7.000     4.400  # vert 18
      6.000     4.000     2.200  # vert 19
      5.000     7.000     3.200  # vert 20
      4.000     7.000     3.200  # vert 21
      4.000     7.000     4.200  # vert 22
      5.000     7.000     4.200  # vert 23
      2.000     7.000     3.200  # vert 24
      1.000     7.000     3.200  # vert 25
      1.000     7.000     4.200  # vert 26
      2.000     7.000     4.200  # vert 27
# no of vertices followed by list of associated vert
   4,  1,  2, 11, 10,
   4,  2,  3, 12, 11,
   4,  3,  4, 13, 12,
   5,  4, 19,  5, 14, 13,
  10,  5,  6, 15, 14,  5, 20, 23, 22, 21, 20,
   4,  6,  7, 16, 15,
   4,  7,  8, 17, 16,
  10,  8,  9, 18, 17,  8, 24, 27, 26, 25, 24,
   4,  9,  1, 10, 18,
   9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
   8,  9,  8,  7,  6,  5, 19,  2,  1,
   4,  2, 19,  4,  3,
   4, 20, 21, 22, 23,
   4, 24, 25, 26, 27,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1  11   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, hall2         OPAQ  VERT  int_part     hall_2         
  2, hall_2_2      OPAQ  VERT  int_part     hall_2         
  3, bed_s         OPAQ  VERT  int_part     bed_south      
  4, east          OPAQ  VERT  ext_part     SIMILAR        
  5, north1        OPAQ  VERT  ext_wall     EXTERIOR       
  6, Surf-6        OPAQ  VERT  int_part     ADIABATIC      
  7, Surf-7        OPAQ  VERT  int_part     ADIABATIC      
  8, north2        OPAQ  VERT  ext_wall     EXTERIOR       
  9, east          OPAQ  VERT  ext_part     SIMILAR        
 10, Surf-10       OPAQ  CEIL  loft_ceil    loft           
 11, Surf-11       OPAQ  FLOR  upper_flor   liv_room       
 12, kitch         OPAQ  FLOR  upper_flor   kitchen        
 13, window1       TRAN  VERT  single_glaz  EXTERIOR       
 14, window2       TRAN  VERT  single_glaz  EXTERIOR       
