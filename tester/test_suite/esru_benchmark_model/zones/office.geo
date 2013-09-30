# geometry of office defined in: ../zones/office.geo
GEN  office  office describes a...  # type, name, descr
      16       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      1.00000     5.00000     0.00000  # vert   1
      5.00000     5.00000     0.00000  # vert   2
      5.00000     9.00000     0.00000  # vert   3
      1.00000     9.00000     0.00000  # vert   4
      1.00000     5.00000     3.00000  # vert   5
      5.00000     5.00000     3.00000  # vert   6
      5.00000     9.00000     3.00000  # vert   7
      1.00000     9.00000     3.00000  # vert   8
      5.00000     6.00000     0.00000  # vert   9
      5.00000     7.00000     0.00000  # vert  10
      5.00000     7.00000     2.50000  # vert  11
      5.00000     6.00000     2.50000  # vert  12
      1.00000     8.00000     1.00000  # vert  13
      1.00000     6.00000     1.00000  # vert  14
      1.00000     6.00000     2.25000  # vert  15
      1.00000     8.00000     2.25000  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   8,  2,  9, 12, 11, 10,  3,  7,  6,
   4,  3,  4,  8,  7,
  10,  4,  1,  5,  8,  4, 13, 16, 15, 14, 13,
   4,  5,  6,  7,  8,
   6,  1,  4,  3, 10,  9,  2,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, part_b        OPAQ  VERT  gyp_gyp_ptn  reception      
  2, part_a        OPAQ  VERT  gyp_gyp_ptn  reception      
  3, North_w       OPAQ  VERT  extern_wall  EXTERIOR       
  4, West_w        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Ceiling       OPAQ  CEIL  ceiling      roof_space     
  6, Floor         OPAQ  FLOR  floor_1      GROUND         
  7, door_a        OPAQ  VERT  door         reception      
  8, west_glz      TRAN  VERT  d_glz        EXTERIOR       
# base
  6  0  0  0  0  0    16.00
