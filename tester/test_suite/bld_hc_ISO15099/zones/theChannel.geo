# geometry of TheChannel defined in: ../zones/theChannel.geo
GEN  TheChannel  TheChannel describes a double facade cavity with air flow  # type, name, descr
      10       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000    -0.75000     0.00000  # vert   1
      4.50000    -0.75000     0.00000  # vert   2
      4.50000    -0.05000     0.00000  # vert   3
      0.00000    -0.05000     0.00000  # vert   4
      0.00000    -0.75000     4.25000  # vert   5
      4.50000    -0.75000     4.25000  # vert   6
      4.50000    -0.05000     4.25000  # vert   7
      0.00000    -0.05000     4.25000  # vert   8
      0.00000    -0.05000     3.50000  # vert   9
      4.50000    -0.05000     3.50000  # vert  10
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   5,  2,  3, 10,  7,  6,
   4,  3,  4,  9, 10,
   5,  4,  1,  5,  8,  9,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4, 10,  9,  8,  7,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, CurtGlass     TRAN  VERT  d_glz        EXTERIOR       
  2, Wall-2        OPAQ  VERT  door         SIMILAR        
  3, iCW           TRAN  VERT  dbl_glz      ANOTHER        
  4, Wall-4        OPAQ  VERT  door         SIMILAR        
  5, Top-5         OPAQ  CEIL  door         ANOTHER        
  6, Base-6        OPAQ  FLOR  door         ANOTHER        
  7, panelCW       OPAQ  VERT  dummy_pnls   ANOTHER        
# base
  6  0  0  0  0  0     3.15 0
