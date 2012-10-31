# geometry of bottom defined in: bottom.geo
GEN  bottom                 # type   zone name
       8       6   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000     0.00000     0.00000  # vert   1
      3.00000     0.00000     0.00000  # vert   2
      3.00000     9.00000     0.00000  # vert   3
      0.00000     9.00000     0.00000  # vert   4
      0.00000     0.00000     3.00000  # vert   5
      3.00000     0.00000     3.00000  # vert   6
      3.00000     9.00000     3.00000  # vert   7
      0.00000     9.00000     3.00000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# number of default windows within each surface 
   0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, south         TRAN  VERT  sgl_glz      EXTERIOR       
  2, east          OPAQ  VERT  extern_wall  ADIABATIC      
  3, north         OPAQ  VERT  extern_wall  EXTERIOR       
  4, west          OPAQ  VERT  extern_wall  ADIABATIC      
  5, ceiling       OPAQ  CEIL  partition    EXTERIOR       
  6, floor         OPAQ  FLOR  partition    EXTERIOR       
# base
   6   0   0   0    27.00
