# geometry of zone_S defined in: ../zones/zone_S.geo
GEN  zone_S  zone_S describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.00000     0.00000     0.00000  # vert   1
      5.50000     0.00000     0.00000  # vert   2
      5.50000     3.54000     0.00000  # vert   3
      3.00000     3.54000     0.00000  # vert   4
      3.00000     0.00000     2.80000  # vert   5
      5.50000     0.00000     2.80000  # vert   6
      5.50000     3.54000     2.80000  # vert   7
      3.00000     3.54000     2.80000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  int_part     zone_L         
  4, Surf-4        OPAQ  VERT  int_part     room           
  5, ceiling       OPAQ  CEIL  ceiling      roof_2         
  6, floor         OPAQ  FLOR  floor_1      GROUND         
# base
  6  0  0  0  0  0     8.85 0
