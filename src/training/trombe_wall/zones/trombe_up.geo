# geometry of trombe_up defined in: ../zones/trombe_up.geo
GEN  trombe_up  trombe_up is the clear glazing section of trombe wall air gap  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     1.00000     2.25000  # vert   1
      4.00000     1.00000     2.25000  # vert   2
      4.00000     1.20000     2.25000  # vert   3
      0.00000     1.20000     2.25000  # vert   4
      0.00000     1.00000     3.00000  # vert   5
      4.00000     1.00000     3.00000  # vert   6
      4.00000     1.20000     3.00000  # vert   7
      0.00000     1.20000     3.00000  # vert   8
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
  1, front         TRAN  VERT  single_glz   EXTERIOR       
  2, right         OPAQ  VERT  insul_mtl_p  SIMILAR        
  3, frame_open    OPAQ  VERT  door         ANOTHER        
  4, fict_left     TRAN  VERT  fictitious   ANOTHER        
  5, top           OPAQ  CEIL  insul_mtl_p  EXTERIOR       
  6, fict_base     TRAN  FLOR  fictitious   ANOTHER        
# base
  6  0  0  0  0  0     0.80 0
