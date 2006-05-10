# geometry of loft defined in: ../zones/loft.geo
GEN  loft  loft is the roof space  # type, name, descr
      17      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.00000     7.00000     4.40000  # vert   1
      0.00000     7.00000     4.40000  # vert   2
      0.00000     0.00000     4.40000  # vert   3
      6.00000     0.00000     4.40000  # vert   4
      6.00000     3.50000     6.20000  # vert   5
      0.00000     3.50000     6.20000  # vert   6
      6.00000     5.00000     4.40000  # vert   7
      3.00000     5.00000     4.40000  # vert   8
      3.00000     7.00000     4.40000  # vert   9
      0.00000     4.00000     4.40000  # vert  10
      3.00000     4.00000     4.40000  # vert  11
      3.00000     0.00000     4.40000  # vert  12
      6.00000     3.00000     4.40000  # vert  13
      3.00000     3.00000     4.40000  # vert  14
      6.00000     4.00000     4.40000  # vert  15
      4.50000     4.00000     4.40000  # vert  16
      4.50000     5.00000     4.40000  # vert  17
# no of vertices followed by list of associated vert
   5,  7, 17,  8,  9,  1,
   6,  4, 13, 15,  7,  1,  5,
   4,  3,  6,  2, 10,
   5,  1,  9,  2,  6,  5,
   5,  4,  5,  6,  3, 12,
   5,  3, 10, 11, 14, 12,
   5,  9,  8, 11, 10,  2,
   7, 13, 14, 11,  8, 17, 16, 15,
   4,  7, 15, 16, 17,
   4, 12, 14, 13,  4,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, bathrm        OPAQ  FLOR  loft_floor   bathrm         
  2, east          OPAQ  VERT  ext_part     SIMILAR        
  3, west          OPAQ  VERT  ext_part     SIMILAR        
  4, north         OPAQ  SLOP  roof         EXTERIOR       
  5, south         OPAQ  SLOP  roof         EXTERIOR       
  6, bed2          OPAQ  FLOR  loft_floor   bed2           
  7, bed1          OPAQ  FLOR  loft_floor   bed1           
  8, hall2         OPAQ  FLOR  loft_floor   hall_2         
  9, w_heat_c      OPAQ  FLOR  loft_floor   w_heat_c       
 10, bed3          OPAQ  FLOR  loft_floor   bed3           
# base
   1   6   7   8    31.50
