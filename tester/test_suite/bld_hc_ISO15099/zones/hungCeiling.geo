# geometry of hungCeiling defined in: ../zones/hungCeiling.geo
GEN  hungCeiling  hungCeiling describes the void of the hung ceiling  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     3.55000  # vert   1
      4.50000     0.00000     3.55000  # vert   2
      4.50000     6.00000     3.55000  # vert   3
      0.00000     6.00000     3.55000  # vert   4
      0.00000     0.00000     4.25000  # vert   5
      4.50000     0.00000     4.25000  # vert   6
      4.50000     6.00000     4.25000  # vert   7
      0.00000     6.00000     4.25000  # vert   8
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
  1, panelCW       OPAQ  VERT  SinkPanel    ANOTHER        
  2, Wall-2        OPAQ  VERT  partition    SIMILAR        
  3, Wall-3        OPAQ  VERT  partition    SIMILAR        
  4, Wall-4        OPAQ  VERT  partition    SIMILAR        
  5, Ceil          OPAQ  CEIL  susp_flr_re  ANOTHER        
  6, hungCeil      OPAQ  FLOR  ceil_white   ANOTHER        
# base
  6  0  0  0  0  0    27.00 0
