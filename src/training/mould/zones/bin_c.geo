# geometry of bin_c defined in: bin_c.geo
GEN  bin_c                  # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.000     0.000     0.000  # vert  1
      1.500     0.000     0.000  # vert  2
      1.500     1.500     0.000  # vert  3
      0.000     1.500     0.000  # vert  4
      0.000     0.000     2.200  # vert  5
      1.500     0.000     2.200  # vert  6
      1.500     1.500     2.200  # vert  7
      0.000     1.500     2.200  # vert  8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
# number of default windows within each surface 
  0, 0, 0, 0, 0, 0,
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  ext_wall     EXTERIOR       
  2, porch         OPAQ  VERT  int_doors    EXTERIOR       
  3, stairs        OPAQ  VERT  ext_part     stairs         
  4, west          OPAQ  VERT  ext_part     SIMILAR        
  5, ceiling       OPAQ  CEIL  ceiling      bathrm         
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
