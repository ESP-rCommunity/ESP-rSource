# geometry of roof_2 defined in: ../zones/roof_2.geo
GEN  roof_2  roof_2 describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.00000     0.00000     2.80000  # vert   1
      5.50000     0.00000     2.80000  # vert   2
      5.50000     7.50000     2.80000  # vert   3
      3.00000     7.50000     2.80000  # vert   4
      4.50000     0.00000     3.80000  # vert   5
      4.50000     7.50000     3.80000  # vert   6
      3.00000     3.54000     2.80000  # vert   7
      5.50000     3.54000     2.80000  # vert   8
# no of vertices followed by list of associated vert
   3,  1,  2,  5,
   5,  2,  8,  3,  6,  5,
   3,  3,  4,  6,
   5,  4,  7,  1,  5,  6,
   4,  1,  7,  8,  2,
   4,  8,  7,  4,  3,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  roof_1       EXTERIOR       
  2, Surf_2        OPAQ  SLOP  roof_2       EXTERIOR       
  3, Surf-3        OPAQ  VERT  roof_1       EXTERIOR       
  4, Surf-4        OPAQ  SLOP  roof_2       EXTERIOR       
  5, ceil_1        OPAQ  FLOR  ceiling_rev  zone_S         
  6, ceil_2        OPAQ  FLOR  ceiling_rev  zone_L         
# base
  5  6  0  0  0  0    18.75 0
