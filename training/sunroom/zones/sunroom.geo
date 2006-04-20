# geometry of sunroom defined in: ../zones/sunroom.geo
GEN  sunroom  sunroom describes the only zone in the problem  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      3.00000     0.00000     0.00000  # vert   2
      3.00000     4.00000     0.00000  # vert   3
      0.00000     4.00000     0.00000  # vert   4
      0.00000     0.00000     2.70000  # vert   5
      3.00000     0.00000     2.70000  # vert   6
      3.00000     4.00000     2.70000  # vert   7
      0.00000     4.00000     2.70000  # vert   8
      0.50000     0.00000     0.50000  # vert   9
      2.50000     0.00000     0.50000  # vert  10
      2.50000     0.00000     2.20000  # vert  11
      0.50000     0.00000     2.20000  # vert  12
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
# unused index
  0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   6    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, sud           OPAQ  VERT  typical_wall EXTERIOR       
  2, est           OPAQ  VERT  typical_wall EXTERIOR       
  3, nord          OPAQ  VERT  typical_wall EXTERIOR       
  4, ouest         OPAQ  VERT  typical_wall EXTERIOR       
  5, haut          OPAQ  CEIL  typical_wall EXTERIOR       
  6, bas           OPAQ  FLOR  typical_wall EXTERIOR       
  7, fenetre       TRAN  VERT  d_glz        EXTERIOR       
# base
  6  0  0  0  0  0    12.00
