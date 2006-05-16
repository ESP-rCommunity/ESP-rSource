# geometry of roof_2 defined in: roof_2.geo
GEN  roof_2                 # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      3.000     0.000     2.800  # vert  1
      5.500     0.000     2.800  # vert  2
      5.500     7.500     2.800  # vert  3
      3.000     7.500     2.800  # vert  4
      4.500     0.000     3.800  # vert  5
      4.500     7.500     3.800  # vert  6
      3.000     3.540     2.800  # vert  7
      5.500     3.540     2.800  # vert  8
# no of vertices followed by list of associated vert
   3,  1,  2,  5,
   4,  2,  3,  6,  5,
   3,  3,  4,  6,
   4,  4,  1,  5,  6,
   4,  1,  7,  8,  2,
   4,  8,  7,  4,  3,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  roof_1       EXTERIOR       
  2, Surf_2        OPAQ  VERT  roof_2       EXTERIOR       
  3, Surf-3        OPAQ  VERT  roof_1       EXTERIOR       
  4, Surf-4        OPAQ  VERT  roof_2       EXTERIOR       
  5, ceil_1        OPAQ  FLOR  ceiling_rev  zone_S         
  6, ceil_2        OPAQ  FLOR  ceiling_rev  zone_L         
