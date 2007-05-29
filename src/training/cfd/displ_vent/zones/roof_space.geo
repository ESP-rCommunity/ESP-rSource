# geometry of roof_space defined in: ../zones/roof_space.geo
GEN  roof_space  roof_space describes a...  # type, name, descr
      13       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     3.00000  # vert   1
      9.00000     0.00000     3.00000  # vert   2
      9.00000     9.00000     3.00000  # vert   3
      0.00000     9.00000     3.00000  # vert   4
      5.00000     9.00000     3.00000  # vert   5
      0.00000     5.00000     3.00000  # vert   6
      9.00000     5.00000     4.50000  # vert   7
      9.00000     9.00000     4.50000  # vert   8
      5.00000     5.00000     3.00000  # vert   9
      9.00000     1.00000     3.00000  # vert  10
      1.00000     1.00000     3.00000  # vert  11
      1.00000     5.00000     3.00000  # vert  12
      1.00000     9.00000     3.00000  # vert  13
# no of vertices followed by list of associated vert
   6, 12,  9,  5,  3, 10, 11,
   4, 13,  5,  9, 12,
   3,  1,  2,  7,
   5,  2, 10,  3,  8,  7,
   5,  3,  5, 13,  4,  8,
   5,  1,  7,  8,  4,  6,
   8,  4, 13, 12, 11, 10,  2,  1,  6,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Ceiling_r     OPAQ  FLOR  ceiling_rev  reception      
  2, Ceiling_o     OPAQ  FLOR  ceiling_rev  office         
  3, s_roof        OPAQ  SLOP  roof_1       EXTERIOR       
  4, Fire_wall     OPAQ  VERT  extern_wall  CONSTANT       
  5, North_wall    OPAQ  VERT  extern_wall  EXTERIOR       
  6, w_roof        OPAQ  SLOP  roof_1       EXTERIOR       
  7, soffit        OPAQ  FLOR  ceiling_rev  EXTERIOR       
# base
  0  0  0  0  0  0    81.00 1
