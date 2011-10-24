# geometry of bypass defined in: ../zones/bypass.geo
GEN  bypass  bypass describes a vertical air channel for circulation  # type, name, descr
      14       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     -0.20000     1.00000     0.00000  # vert   1
      0.00000     1.00000     0.00000  # vert   2
      0.00000     1.20000     0.00000  # vert   3
     -0.20000     1.20000     0.00000  # vert   4
     -0.20000     1.00000     3.00000  # vert   5
      0.00000     1.00000     3.00000  # vert   6
      0.00000     1.20000     3.00000  # vert   7
     -0.20000     1.20000     3.00000  # vert   8
      0.00000     1.00000     0.75000  # vert   9
      0.00000     1.20000     0.75000  # vert  10
      0.00000     1.00000     1.50000  # vert  11
      0.00000     1.20000     1.50000  # vert  12
      0.00000     1.00000     2.25000  # vert  13
      0.00000     1.20000     2.25000  # vert  14
# no of vertices followed by list of associated vert
   7,  1,  2,  9, 11, 13,  6,  5,
   7,  3,  4,  8,  7, 14, 12, 10,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  2,  3, 10,  9,
   4,  9, 10, 12, 11,
   4, 11, 12, 14, 13,
   4, 13, 14,  7,  6,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, front         OPAQ  VERT  insul_mtl_p  EXTERIOR       
  2, back          OPAQ  VERT  insul_mtl_p  ADIABATIC      
  3, left          OPAQ  VERT  insul_mtl_p  ADIABATIC      
  4, top           OPAQ  CEIL  insul_mtl_p  ADIABATIC      
  5, base          OPAQ  FLOR  insul_mtl_p  ADIABATIC      
  6, fict_low      TRAN  VERT  fictitious   ANOTHER        
  7, fict_mid      TRAN  VERT  fictitious   ANOTHER        
  8, fict_mid2     TRAN  VERT  fictitious   ANOTHER        
  9, fict_up       TRAN  VERT  fictitious   ANOTHER        
# base
  6  5  0  0  0  0     0.19 0
