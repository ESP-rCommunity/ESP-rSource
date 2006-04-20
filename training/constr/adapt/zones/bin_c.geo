# geometry of bin_c defined in: ../zones/bin_c.geo
GEN  bin_c  bin_c describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      6.00000     7.00000     0.00000  # vert   1
      4.50000     7.00000     0.00000  # vert   2
      4.50000     5.50000     0.00000  # vert   3
      6.00000     5.50000     0.00000  # vert   4
      6.00000     7.00000     2.20000  # vert   5
      4.50000     7.00000     2.20000  # vert   6
      4.50000     5.50000     2.20000  # vert   7
      6.00000     5.50000     2.20000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
# number of default windows within each surface 
   0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, north         OPAQ  VERT  ext_wall     EXTERIOR       
  2, porch         OPAQ  VERT  int_doors    EXTERIOR       
  3, stairs        OPAQ  VERT  ext_part     stairs         
  4, east          OPAQ  VERT  ext_part     SIMILAR        
  5, bathrm        OPAQ  CEIL  ceiling      bathrm         
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
# base
   6   0   0   0     2.25
