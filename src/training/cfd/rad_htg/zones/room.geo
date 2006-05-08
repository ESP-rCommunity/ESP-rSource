# geometry of room defined in: room.geo
GEN  room                   # type   zone name
      20      11   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000     0.00000     0.00000  # vert  1
      3.00000     0.00000     0.00000  # vert  2
      3.00000     5.00000     0.00000  # vert  3
      0.00000     5.00000     0.00000  # vert  4
      0.00000     0.00000     2.80000  # vert  5
      3.00000     0.00000     2.80000  # vert  6
      3.00000     5.00000     2.80000  # vert  7
      0.00000     5.00000     2.80000  # vert  8
      2.00000     5.00000     0.00000  # vert  9
      1.00000     5.00000     0.00000  # vert 10
      1.00000     5.00000     1.00000  # vert 11
      2.00000     5.00000     1.00000  # vert 12
      1.00000     0.00000     1.00000  # vert 13
      2.00000     0.00000     1.00000  # vert 14
      2.00000     0.00000     2.20000  # vert 15
      1.00000     0.00000     2.20000  # vert 16
      3.00000     3.54000     0.00000  # vert 17
      3.00000     3.54000     2.80000  # vert 18
      1.00000     5.00000     2.80000  # vert 19
      2.00000     5.00000     2.80000  # vert 20
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1, 13, 16, 15, 14, 13,
   4,  2, 17, 18,  6,
   5,  3,  9, 12, 20,  7,
   4,  4,  1,  5,  8,
   7,  5,  6, 18,  7, 20, 19,  8,
   7,  1,  4, 10,  9,  3, 17,  2,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
   4, 17,  3,  7, 18,
   4, 12, 11, 19, 20,
   5, 10,  4,  8, 19, 11,
# number of default windows within each surface 
   0  0  0  0  0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, part-1a       OPAQ  VERT  int_part     zone_S         
  3, part-2b       OPAQ  VERT  int_part     zone_L         
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, ceiling       OPAQ  CEIL  ceiling      roof_1         
  6, floor         OPAQ  FLOR  floor_1      GROUND         
  7, radiator      OPAQ  VERT  int_part     zone_L         
  8, window        TRAN  VERT  d_glz        EXTERIOR       
  9, part-2a       OPAQ  VERT  int_part     zone_L         
 10, part-2c       OPAQ  VERT  int_part     zone_L         
 11, part-2d       OPAQ  VERT  int_part     zone_L         
