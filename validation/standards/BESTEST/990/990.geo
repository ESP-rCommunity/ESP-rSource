# geometry of 990 defined in: ../990/990.geo
GEN  990  990 describes a...  # type, name, descr
      12      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000    -1.35000  # vert   1
      8.00000     0.00000    -1.35000  # vert   2
      8.00000     6.00000    -1.35000  # vert   3
      0.00000     6.00000    -1.35000  # vert   4
      0.00000     0.00000     0.00000  # vert   5
      8.00000     0.00000     0.00000  # vert   6
      8.00000     6.00000     0.00000  # vert   7
      0.00000     6.00000     0.00000  # vert   8
      0.00000     0.00000     1.35000  # vert   9
      8.00000     0.00000     1.35000  # vert  10
      8.00000     6.00000     1.35000  # vert  11
      0.00000     6.00000     1.35000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6, 10,  9,
   4,  6,  7, 11, 10,
   4,  7,  8, 12, 11,
   4,  8,  5,  9, 12,
   4,  9, 10, 11, 12,
   4,  4,  3,  2,  1,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1  10   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, S-Ground      OPAQ  VERT  GroundWall   GROUND         
  2, E-Ground      OPAQ  VERT  GroundWall   GROUND         
  3, N-Ground      OPAQ  VERT  GroundWall   GROUND         
  4, W-Ground      OPAQ  VERT  GroundWall   GROUND         
  5, Window        TRAN  VERT  dg_glazing   EXTERIOR       
  6, E-Wall        OPAQ  VERT  HW_extwall   EXTERIOR       
  7, N-Wall        OPAQ  VERT  HW_extwall   EXTERIOR       
  8, W-Wall        OPAQ  VERT  HW_extwall   EXTERIOR       
  9, Roof          OPAQ  CEIL  HWroof       EXTERIOR       
 10, Floor         OPAQ  FLOR  HWfloor      GROUND         
# base
 10  0  0  0  0  0    48.00
