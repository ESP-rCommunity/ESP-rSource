# geometry of reception defined in: ../zones/reception.geo
GEN  reception  reception describes a...  # type, name, descr
      34      14   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      1.00000     1.00000     0.00000  # vert   1
      9.00000     1.00000     0.00000  # vert   2
      9.00000     4.50000     0.00000  # vert   3
      9.00000     9.00000     0.00000  # vert   4
      5.00000     9.00000     0.00000  # vert   5
      5.00000     5.00000     0.00000  # vert   6
      1.00000     5.00000     0.00000  # vert   7
      1.00000     1.00000     3.00000  # vert   8
      9.00000     1.00000     3.00000  # vert   9
      9.00000     4.50000     3.00000  # vert  10
      9.00000     9.00000     3.00000  # vert  11
      5.00000     9.00000     3.00000  # vert  12
      5.00000     5.00000     3.00000  # vert  13
      1.00000     5.00000     3.00000  # vert  14
      2.00000     1.00000     1.00000  # vert  15
      8.00000     1.00000     1.00000  # vert  16
      8.00000     1.00000     2.25000  # vert  17
      2.00000     1.00000     2.25000  # vert  18
      9.00000     5.00000     0.00000  # vert  19
      9.00000     6.00000     0.00000  # vert  20
      9.00000     6.00000     2.50000  # vert  21
      9.00000     5.00000     2.50000  # vert  22
      5.00000     7.00000     0.00000  # vert  23
      5.00000     6.00000     0.00000  # vert  24
      5.00000     6.00000     2.50000  # vert  25
      5.00000     7.00000     2.50000  # vert  26
      1.00000     3.00000     0.00000  # vert  27
      1.00000     2.00000     0.00000  # vert  28
      1.00000     2.00000     2.50000  # vert  29
      1.00000     3.00000     2.50000  # vert  30
      9.00000     2.00000     1.00000  # vert  31
      9.00000     4.00000     1.00000  # vert  32
      9.00000     4.00000     2.25000  # vert  33
      9.00000     2.00000     2.25000  # vert  34
# no of vertices followed by list of associated vert
  10,  1,  2,  9,  8,  1, 15, 18, 17, 16, 15,
  10,  2,  3, 10,  9,  2, 31, 34, 33, 32, 31,
   8,  3, 19, 22, 21, 20,  4, 11, 10,
   4,  4,  5, 12, 11,
   8,  5, 23, 26, 25, 24,  6, 13, 12,
   4,  6,  7, 14, 13,
   8,  7, 27, 30, 29, 28,  1,  8, 14,
   7,  8,  9, 10, 11, 12, 13, 14,
  13,  1, 28, 27,  7,  6, 24, 23,  5,  4, 20, 19,  3,  2,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
   4, 23, 24, 25, 26,
   4, 27, 28, 29, 30,
   4, 31, 32, 33, 34,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  extern_wall  EXTERIOR       
  2, east          OPAQ  VERT  extern_wall  EXTERIOR       
  3, pasg          OPAQ  VERT  gyp_blk_ptn  SIMILAR        
  4, north         OPAQ  VERT  extern_wall  EXTERIOR       
  5, part_a        OPAQ  VERT  gyp_gyp_ptn  office         
  6, part_b        OPAQ  VERT  gyp_gyp_ptn  office         
  7, west          OPAQ  VERT  extern_wall  EXTERIOR       
  8, ceiling       OPAQ  CEIL  ceiling      roof_space     
  9, floor         OPAQ  FLOR  floor_1      CONSTANT       
 10, glz_s         TRAN  VERT  dbl_glz      EXTERIOR       
 11, door_p        OPAQ  VERT  door         SIMILAR        
 12, door_a        OPAQ  VERT  door         office         
 13, door_w        OPAQ  VERT  door         EXTERIOR       
 14, east_glz      TRAN  VERT  dbl_glz      EXTERIOR       
# base
  9  0  0  0  0  0    48.00 0
