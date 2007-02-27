# geometry of tube_a defined in: ../zones/tube_a.geo
GEN  tube_a  tube_a describes the a section of earth tube  # type, name, descr
      12       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     -3.96110    -3.18252    -1.00000  # vert   1
      7.50000    -3.00000    -1.00000  # vert   2
      7.50000    -2.00000    -1.00000  # vert   3
      7.00000    -2.00000    -1.00000  # vert   4
      7.00000    -2.50000    -1.00000  # vert   5
     -3.99333    -2.69990    -1.00000  # vert   6
     -3.96110    -3.18252    -0.80000  # vert   7
      7.50000    -3.00000    -0.80000  # vert   8
      7.50000    -2.00000    -0.80000  # vert   9
      7.00000    -2.00000    -0.80000  # vert  10
      7.00000    -2.50000    -0.80000  # vert  11
     -3.99333    -2.69990    -0.80000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  8,  7,
   4,  2,  3,  9,  8,
   4,  3,  4, 10,  9,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
   4,  6,  1,  7, 12,
   6,  7,  8,  9, 10, 11, 12,
   6,  1,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, side_a        OPAQ  VERT  earth_side   GROUND         
  2, side_b        OPAQ  VERT  earth_side   GROUND         
  3, side_c        OPAQ  VERT  insul_frame  tube_b         
  4, side_e        OPAQ  VERT  earth_side   GROUND         
  5, side_f        OPAQ  VERT  earth_side   GROUND         
  6, side_g        OPAQ  VERT  earth_side   GROUND         
  7, top           OPAQ  CEIL  earth_top    EXTERIOR       
  8, base          OPAQ  FLOR  below        GROUND         
# base
  8  0  0  0  0  0     5.94 0
