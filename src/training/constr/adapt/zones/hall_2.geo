# geometry of hall_2 defined in: ../zones/hall_2.geo
GEN  hall_2  hall_2 describes the upper level stair landing  # type, name, descr
      17      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.50000     5.00000     2.20000  # vert   1
      3.00000     5.00000     2.20000  # vert   2
      3.00000     3.00000     2.20000  # vert   3
      6.00000     3.00000     2.20000  # vert   4
      6.00000     4.00000     2.20000  # vert   5
      4.50000     4.00000     2.20000  # vert   6
      4.50000     5.00000     4.40000  # vert   7
      3.00000     5.00000     4.40000  # vert   8
      3.00000     3.00000     4.40000  # vert   9
      6.00000     3.00000     4.40000  # vert  10
      6.00000     4.00000     4.40000  # vert  11
      4.50000     4.00000     4.40000  # vert  12
      3.00000     4.00000     2.20000  # vert  13
      3.00000     4.00000     4.40000  # vert  14
      4.50000     3.00000     2.20000  # vert  15
      4.00000     3.00000     2.20000  # vert  16
      4.00000     3.00000     4.40000  # vert  17
# no of vertices followed by list of associated vert
   4,  1,  2,  8,  7,
   4,  2, 13, 14,  8,
   5, 16, 15,  4, 10, 17,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
   4,  6,  1,  7, 12,
   8,  7,  8, 14,  9, 17, 10, 11, 12,
   7,  6, 15, 16,  3, 13,  2,  1,
   4, 13,  3,  9, 14,
   4,  6,  5,  4, 15,
   4,  3, 16, 17,  9,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, door_bath     OPAQ  VERT  int_doors    bathrm         
  2, door_bed1     OPAQ  VERT  int_doors    bed1           
  3, bed3          OPAQ  VERT  int_part     bed3           
  4, east          OPAQ  VERT  ext_part     SIMILAR        
  5, w_heater_c    OPAQ  VERT  int_part     w_heat_c       
  6, door_whc      OPAQ  VERT  int_doors    w_heat_c       
  7, ceiling       OPAQ  CEIL  loft_ceil    loft           
  8, hall1         OPAQ  FLOR  upper_flor   hall_1         
  9, door_bed2     OPAQ  VERT  int_doors    bed2           
 10, stairs        OPAQ  FLOR  int_doors    stairs         
 11, door_bed3     OPAQ  VERT  int_doors    bed3           
# base
   8  10   0   0     4.50
