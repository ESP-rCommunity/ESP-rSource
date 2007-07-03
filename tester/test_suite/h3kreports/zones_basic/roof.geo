# geometry of roof defined in: ../zones/roof.geo
GEN  roof  roof describes the two-level roof of the house  # type, name, descr
      13      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     8.08500  # vert   1
     12.00000     0.00000     8.08500  # vert   2
     12.00000     9.20000     8.08500  # vert   3
      0.00000     9.20000     8.08500  # vert   4
      5.75000     4.60000    11.92000  # vert   5
      6.25000     4.60000    11.92000  # vert   6
     12.00000     1.00000     9.08500  # vert   7
     12.00000     8.20000     9.08500  # vert   8
     12.00000    12.20000     8.08500  # vert   9
      0.00000    12.20000     8.08500  # vert  10
      6.00000     9.20000    10.58500  # vert  11
      6.00000     8.50000    10.58500  # vert  12
      6.00000     6.26680    10.58500  # vert  13
# no of vertices followed by list of associated vert
   3,  4,  1,  5,
   3,  6,  7,  8,
   4,  1,  7,  6,  5,
   4,  2,  3,  8,  7,
   3,  9, 10, 11,
   5, 13, 12, 11, 10,  4,
   5,  3,  9, 11, 12, 13,
   6,  5,  6,  8,  3, 13,  4,
   3,  1,  2,  7,
   6,  3,  2,  1,  4, 10,  9,
# unused index
  0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, roof1_back    OPAQ  SLOP  asphalt      EXTERIOR       
  2, roof1_frnt    OPAQ  SLOP  asphalt      EXTERIOR       
  3, roof1_side    OPAQ  SLOP  asphalt      EXTERIOR       
  4, roof1_vert    OPAQ  VERT  asphalt      EXTERIOR       
  5, roof2_vert    OPAQ  SLOP  asphalt      EXTERIOR       
  6, roof2_back    OPAQ  SLOP  asphalt      EXTERIOR       
  7, roof2_front   OPAQ  SLOP  asphalt      EXTERIOR       
  8, roof_1_2      OPAQ  SLOP  asphalt      EXTERIOR       
  9, roof1_tri     OPAQ  SLOP  asphalt      EXTERIOR       
 10, ceiling       OPAQ  FLOR  ceiling_r    main_second    
# base
  10   0   0   0   0   0   142.68
