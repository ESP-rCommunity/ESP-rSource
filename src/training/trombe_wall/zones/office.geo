# geometry of office defined in: ../zones/office.geo
GEN  office  office is behind trombe wall with clear story glazing  # type, name, descr
      18      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     1.70000     0.00000  # vert   1
      4.00000     1.70000     0.00000  # vert   2
      4.00000     5.70000     0.00000  # vert   3
      0.00000     5.70000     0.00000  # vert   4
      0.00000     1.70000     3.00000  # vert   5
      4.00000     1.70000     3.00000  # vert   6
      4.00000     5.70000     3.00000  # vert   7
      0.00000     5.70000     3.00000  # vert   8
      0.00000     1.20000     2.25000  # vert   9
      0.00000     1.20000     3.00000  # vert  10
      4.00000     1.20000     2.25000  # vert  11
      4.00000     1.20000     3.00000  # vert  12
      0.00000     1.70000     2.25000  # vert  13
      4.00000     1.70000     2.25000  # vert  14
      0.00000     1.70000     0.75000  # vert  15
      0.00000     1.70000     1.50000  # vert  16
      4.00000     1.70000     0.75000  # vert  17
      4.00000     1.70000     1.50000  # vert  18
# no of vertices followed by list of associated vert
   4,  1,  2, 17, 15,
   9,  2,  3,  7,  6, 12, 11, 14, 18, 17,
   4,  3,  4,  8,  7,
   9,  4,  1, 15, 16, 13,  9, 10,  5,  8,
   6, 10, 12,  6,  7,  8,  5,
   4,  1,  4,  3,  2,
   4,  9, 11, 12, 10,
   4, 15, 17, 18, 16,
   4, 16, 18, 14, 13,
   4, 11,  9, 13, 14,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, wall_tlow     OPAQ  VERT  trombe_mass  ANOTHER        
  2, right_ptn     OPAQ  VERT  gyp_blk_ptn  SIMILAR        
  3, back_ptn      OPAQ  VERT  gyp_blk_ptn  SIMILAR        
  4, left_ptn      OPAQ  VERT  gyp_blk_ptn  SIMILAR        
  5, ceiling       OPAQ  CEIL  trombe_mass  SIMILAR        
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, open_frame    OPAQ  VERT  door         ANOTHER        
  8, wall_tmid     OPAQ  VERT  trombe_mass  ANOTHER        
  9, wall_tup      OPAQ  VERT  trombe_mass  ANOTHER        
 10, top_of_tw     OPAQ  FLOR  trombe_mas_a SIMILAR        
# base
  6 10  0  0  0  0    18.00 0
