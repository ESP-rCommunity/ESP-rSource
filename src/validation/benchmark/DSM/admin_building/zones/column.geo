# geometry of column defined in: ../zones/column.geo
GEN  column  column describes a dummy zone for columns  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.60483    -1.95512    47.95000  # vert   1
      6.55995    -1.98085    47.95000  # vert   2
      6.55995    -0.56596    47.95000  # vert   3
      4.60483    -0.59168    47.95000  # vert   4
      4.60483    -1.95512    49.95000  # vert   5
      6.55995    -1.98085    49.95000  # vert   6
      6.55995    -0.56596    49.95000  # vert   7
      4.60483    -0.59168    49.95000  # vert   8
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
  1, Wall-1        OPAQ  VERT  UNKNOWN      UNKNOWN        
  2, Wall-2        OPAQ  VERT  UNKNOWN      UNKNOWN        
  3, Wall-3        OPAQ  VERT  UNKNOWN      UNKNOWN        
  4, Wall-4        OPAQ  VERT  UNKNOWN      UNKNOWN        
  5, Top-5         OPAQ  CEIL  UNKNOWN      UNKNOWN        
  6, Base-6        OPAQ  FLOR  UNKNOWN      UNKNOWN        
# base
  6  0  0  0  0  0     2.72 0
