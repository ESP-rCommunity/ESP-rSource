# geometry of Test_3 defined in: zones/Test_3.geo
GEN  Test_3  Test_3 describes a...  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      5.50000     0.00000     0.00000  # vert   2
      5.50000     3.60000     0.00000  # vert   3
      0.00000     3.60000     0.00000  # vert   4
      0.00000     0.00000     2.80000  # vert   5
      5.50000     0.00000     2.80000  # vert   6
      5.50000     3.60000     2.80000  # vert   7
      0.00000     3.60000     2.80000  # vert   8
      0.00000     3.20000     0.10000  # vert   9
      0.00000     0.40000     0.10000  # vert  10
      0.00000     0.40000     2.60000  # vert  11
      0.00000     3.20000     2.60000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
  10,  4,  1,  5,  8,  4,  9, 12, 11, 10,  9,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, South         OPAQ  VERT  T_n.2_09em0a ADIABATIC      
  2, East          OPAQ  VERT  T_n.2_09em0a ADIABATIC      
  3, North         OPAQ  VERT  T_n.2_09em0a ADIABATIC      
  4, West          OPAQ  VERT  Typ_n.1_09em EXTERIOR       
  5, Ceiling       OPAQ  CEIL  T_n.4_09em0a ADIABATIC      
  6, Floor         OPAQ  FLOR  T_n4fl09em0a ADIABATIC      
  7, window        TRAN  VERT  WISdblshd09e EXTERIOR       
# base
  6  0  0  0  0  0    19.80 0
