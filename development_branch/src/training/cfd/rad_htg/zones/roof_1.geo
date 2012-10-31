# geometry of roof_1 defined in: ../zones/roof_1.geo
GEN  roof_1  roof_1 describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     2.80000  # vert   1
      3.00000     0.00000     2.80000  # vert   2
      3.00000     5.00000     2.80000  # vert   3
      0.00000     5.00000     2.80000  # vert   4
      1.50000     0.00000     3.80000  # vert   5
      1.50000     7.50000     3.80000  # vert   6
      3.00000     7.50000     2.80000  # vert   7
      0.00000     7.50000     2.80000  # vert   8
# no of vertices followed by list of associated vert
   3,  1,  2,  5,
   5,  2,  3,  7,  6,  5,
   3,  7,  8,  6,
   5,  4,  1,  5,  6,  8,
   4,  1,  4,  3,  2,
   4,  3,  4,  8,  7,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  roof_1       EXTERIOR       
  2, Surf-2        OPAQ  SLOP  roof_2       EXTERIOR       
  3, Surf-3        OPAQ  VERT  roof_1       EXTERIOR       
  4, Surf-4        OPAQ  SLOP  roof_2       EXTERIOR       
  5, ceil_1        OPAQ  FLOR  ceiling_rev  room           
  6, ceil_2        OPAQ  FLOR  ceiling_rev  zone_L         
# base
  0  0  0  0  0  0    22.50 1
