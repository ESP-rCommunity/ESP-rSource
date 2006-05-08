# geometry of roof defined in: ../zones/roof.geo
GEN  roof  roof describes a...  # type, name, descr
      31      16   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.00000     0.00000     6.50000  # vert   1
     34.00000     0.00000     6.50000  # vert   2
     34.00000     9.50000     6.50000  # vert   3
     34.00000    14.50000     6.50000  # vert   4
     34.00000    36.00000     6.50000  # vert   5
     34.00000    45.50000     6.50000  # vert   6
     34.00000    50.50000     6.50000  # vert   7
     34.00000    60.00000     6.50000  # vert   8
     10.00000    60.00000     6.50000  # vert   9
     10.00000    48.00000     6.50000  # vert  10
     10.00000    46.00000     6.50000  # vert  11
     10.00000    38.00000     6.50000  # vert  12
     10.00000    36.00000     6.50000  # vert  13
     10.00000    12.00000     6.50000  # vert  14
     22.00000    12.00000    10.00000  # vert  15
     22.00000    48.00000    10.00000  # vert  16
     12.00000    12.00000     6.50000  # vert  17
     29.00000    12.00000     6.50000  # vert  18
     29.00000     9.50000     6.50000  # vert  19
     29.00000    14.50000     6.50000  # vert  20
     12.00000    36.00000     6.50000  # vert  21
     16.00000    36.00000     6.50000  # vert  22
     29.50000    45.50000     6.50000  # vert  23
     29.50000    48.00000     6.50000  # vert  24
     29.50000    50.50000     6.50000  # vert  25
     16.00000    48.00000     6.50000  # vert  26
     12.00000    48.00000     6.50000  # vert  27
      7.28300    42.13900     6.50000  # vert  28
      7.00000    48.00000     6.50000  # vert  29
      7.00000    36.00000     6.50000  # vert  30
     16.00000    42.00000     9.00000  # vert  31
# no of vertices followed by list of associated vert
   3,  1,  2, 15,
   9,  2,  3,  4,  5,  6,  7,  8, 16, 15,
   3,  8,  9, 16,
   8,  9, 10, 31, 13, 14,  1, 15, 16,
   7, 21, 22,  5,  4, 20, 18, 17,
   7,  1, 14, 17, 18, 19,  3,  2,
   5,  4,  3, 19, 18, 20,
   4, 21, 27, 26, 22,
   8, 10,  9,  8,  7, 25, 24, 26, 27,
   5, 23, 24, 25,  7,  6,
   9, 14, 13, 12, 28, 11, 10, 27, 21, 17,
   7, 30, 29, 10, 11, 28, 12, 13,
   3, 10, 29, 31,
   3, 29, 30, 31,
   3, 30, 13, 31,
   6, 22, 26, 24, 23,  6,  5,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, south         OPAQ  SLOP  roof         EXTERIOR       
  2, east          OPAQ  SLOP  roof         EXTERIOR       
  3, north         OPAQ  SLOP  roof         EXTERIOR       
  4, west          OPAQ  SLOP  roof         EXTERIOR       
  5, ceil_c        OPAQ  FLOR  susp_ceil    Unit_cd        
  6, ceil_e        OPAQ  FLOR  susp_ceil    Unit_e         
  7, stair_de      OPAQ  FLOR  susp_ceil    stair_deij     
  8, ceil_t        OPAQ  FLOR  susp_ceil    toilets        
  9, ceil_a        OPAQ  FLOR  susp_ceil    Unit_a         
 10, stair_ab      OPAQ  FLOR  susp_ceil    stair_abfg     
 11, corid_c       OPAQ  FLOR  susp_ceil    corid_1        
 12, sofit         OPAQ  FLOR  insul_mtl_p  EXTERIOR       
 13, ent_r_n       OPAQ  SLOP  roof         EXTERIOR       
 14, ent_r_w       OPAQ  SLOP  roof         EXTERIOR       
 15, ent_r_s       OPAQ  SLOP  roof         EXTERIOR       
 16, ceil_b        OPAQ  FLOR  susp_ceil    Unit_b         
# base
  0  0  0  0  0  0  1163.00 1
