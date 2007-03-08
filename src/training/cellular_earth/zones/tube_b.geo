# geometry of tube_b defined in: ../zones/tube_b.geo
GEN  tube_b  tube_b is the b section of the earth tube  # type, name, descr
      24      14   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      7.00000    -2.00000    -1.00000  # vert   1
      7.50000    -2.00000    -1.00000  # vert   2
      7.50000    -1.00000    -1.00000  # vert   3
      5.00000    -1.00000    -1.00000  # vert   4
      5.00000    -1.50000    -1.00000  # vert   5
     -3.50000    -1.50000    -1.00000  # vert   6
     -3.50000    -1.00000    -1.00000  # vert   7
     -4.00000    -1.00000    -1.00000  # vert   8
     -4.00000    -2.00000    -1.00000  # vert   9
      5.50000    -2.00000    -1.00000  # vert  10
      5.50000    -1.50000    -1.00000  # vert  11
      7.00000    -1.50000    -1.00000  # vert  12
      7.00000    -2.00000    -0.80000  # vert  13
      7.50000    -2.00000    -0.80000  # vert  14
      7.50000    -1.00000    -0.80000  # vert  15
      5.00000    -1.00000    -0.80000  # vert  16
      5.00000    -1.50000    -0.80000  # vert  17
     -3.50000    -1.50000    -0.80000  # vert  18
     -3.50000    -1.00000    -0.80000  # vert  19
     -4.00000    -1.00000    -0.80000  # vert  20
     -4.00000    -2.00000    -0.80000  # vert  21
      5.50000    -2.00000    -0.80000  # vert  22
      5.50000    -1.50000    -0.80000  # vert  23
      7.00000    -1.50000    -0.80000  # vert  24
# no of vertices followed by list of associated vert
   4,  1,  2, 14, 13,
   4,  2,  3, 15, 14,
   4,  3,  4, 16, 15,
   4,  4,  5, 17, 16,
   4,  5,  6, 18, 17,
   4,  6,  7, 19, 18,
   4,  7,  8, 20, 19,
   4,  8,  9, 21, 20,
   4,  9, 10, 22, 21,
   4, 10, 11, 23, 22,
   4, 11, 12, 24, 23,
   4, 12,  1, 13, 24,
  12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
  12,  1, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, end_a         OPAQ  VERT  insul_frame  tube_a         
  2, side_a        OPAQ  VERT  earth_side   GROUND         
  3, side_b        OPAQ  VERT  earth_side   GROUND         
  4, side_c        OPAQ  VERT  earth_side   GROUND         
  5, side_d        OPAQ  VERT  earth_side   GROUND         
  6, side_e        OPAQ  VERT  earth_side   GROUND         
  7, side_f        OPAQ  VERT  earth_side   tube_c         
  8, side_g        OPAQ  VERT  earth_side   GROUND         
  9, side_h        OPAQ  VERT  earth_side   GROUND         
 10, side_i        OPAQ  VERT  earth_side   GROUND         
 11, side_j        OPAQ  VERT  earth_side   GROUND         
 12, side_k        OPAQ  VERT  earth_side   GROUND         
 13, top           OPAQ  CEIL  earth_top    EXTERIOR       
 14, base          OPAQ  FLOR  below        GROUND         
# base
 14  0  0  0  0  0     6.50 0
