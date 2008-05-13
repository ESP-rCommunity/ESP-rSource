# geometry of mixTop defined in: ../zones/mixTop.geo
GEN  mixTop  mixTop describes the "mixing zone" at top (outlet)  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000    -0.75000     4.30000  # vert   1
      4.50000    -0.75000     4.30000  # vert   2
      4.50000    -0.05000     4.30000  # vert   3
      0.00000    -0.05000     4.30000  # vert   4
      0.00000    -0.75000     4.45000  # vert   5
      4.50000    -0.75000     4.45000  # vert   6
      4.50000    -0.05000     4.45000  # vert   7
      0.00000    -0.05000     4.45000  # vert   8
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
  1, Wall-1        OPAQ  VERT  alu_frame    ADIABATIC      
  2, Wall-2        OPAQ  VERT  alu_frame    ADIABATIC      
  3, Wall-3        OPAQ  VERT  alu_frame    ADIABATIC      
  4, Wall-4        OPAQ  VERT  alu_frame    ADIABATIC      
  5, Top-5         OPAQ  CEIL  alu_frame    ADIABATIC      
  6, Base-6        OPAQ  FLOR  alu_frame    ANOTHER        
# base
  6  0  0  0  0  0     3.15 0
