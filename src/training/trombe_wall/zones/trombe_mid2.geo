# geometry of trombe_mid2 defined in: ../zones/trombe_mid2.geo
GEN  trombe_mid2  trombe_mid2 describes upper middle section of trombe wall gap  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     1.00000     1.50000  # vert   1
      4.00000     1.00000     1.50000  # vert   2
      4.00000     1.20000     1.50000  # vert   3
      0.00000     1.20000     1.50000  # vert   4
      0.00000     1.00000     2.25000  # vert   5
      4.00000     1.00000     2.25000  # vert   6
      4.00000     1.20000     2.25000  # vert   7
      0.00000     1.20000     2.25000  # vert   8
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
  3, trombe_face   OPAQ  VERT  trombe_mas_a ANOTHER        
  4, fict_left     TRAN  VERT  fictitious   ANOTHER        
  5, fict_up       TRAN  CEIL  fictitious   ANOTHER        
  6, fict_base     TRAN  FLOR  fictitious   ANOTHER        
# base
  6  0  0  0  0  0     0.80 0
