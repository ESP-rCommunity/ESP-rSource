# geometry of tube_c defined in: ../zones/tube_c.geo
GEN  tube_c  tube_c is thye c section of the earth tube  # type, name, descr
      20      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     -4.00000    -1.00000    -1.00000  # vert   1
     -3.50000    -1.00000    -1.00000  # vert   2
     -3.50000    -0.50000    -1.00000  # vert   3
      7.50000    -0.50000    -1.00000  # vert   4
      7.50000     1.50000    -1.00000  # vert   5
      3.50000     1.50000    -1.00000  # vert   6
      3.50000     1.00000    -1.00000  # vert   7
      7.00000     1.00000    -1.00000  # vert   8
      7.00000     0.00000    -1.00000  # vert   9
     -4.00000     0.00000    -1.00000  # vert  10
     -4.00000    -1.00000    -0.80000  # vert  11
     -3.50000    -1.00000    -0.80000  # vert  12
     -3.50000    -0.50000    -0.80000  # vert  13
      7.50000    -0.50000    -0.80000  # vert  14
      7.50000     1.50000    -0.80000  # vert  15
      3.50000     1.50000    -0.80000  # vert  16
      3.50000     1.00000    -0.80000  # vert  17
      7.00000     1.00000    -0.80000  # vert  18
      7.00000     0.00000    -0.80000  # vert  19
     -4.00000     0.00000    -0.80000  # vert  20
# no of vertices followed by list of associated vert
   4,  1,  2, 12, 11,
   4,  2,  3, 13, 12,
   4,  3,  4, 14, 13,
   4,  4,  5, 15, 14,
   4,  5,  6, 16, 15,
   4,  6,  7, 17, 16,
   4,  7,  8, 18, 17,
   4,  8,  9, 19, 18,
   4,  9, 10, 20, 19,
   4, 10,  1, 11, 20,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  10,  1, 10,  9,  8,  7,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, inlet         OPAQ  VERT  insul_frame  tube_b         
  2, side_a        OPAQ  VERT  earth_side   GROUND         
  3, side_b        OPAQ  VERT  earth_side   GROUND         
  4, side_c        OPAQ  VERT  earth_side   GROUND         
  5, side_d        OPAQ  VERT  earth_side   GROUND         
  6, outlet        OPAQ  VERT  insul_frame  GROUND         
  7, side_e        OPAQ  VERT  earth_side   GROUND         
  8, side_f        OPAQ  VERT  earth_side   GROUND         
  9, side_g        OPAQ  VERT  earth_side   GROUND         
 10, side_h        OPAQ  VERT  earth_side   GROUND         
 11, top           OPAQ  CEIL  earth_top    EXTERIOR       
 12, base          OPAQ  FLOR  below        GROUND         
# base
 12  0  0  0  0  0     8.50 0
