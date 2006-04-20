# geometry of ceil_fg defined in: ../zones/ceil_fg.geo
GEN  ceil_fg  ceil_fg describes a...  # type, name, descr
      24      16   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     16.00000    36.00000     2.70000  # vert   1
     34.00000    36.00000     2.70000  # vert   2
     34.00000    45.50000     2.70000  # vert   3
     29.50000    45.50000     2.70000  # vert   4
     29.50000    48.00000     2.70000  # vert   5
     29.50000    50.50000     2.70000  # vert   6
     34.00000    50.50000     2.70000  # vert   7
     34.00000    60.00000     2.70000  # vert   8
     10.00000    60.00000     2.70000  # vert   9
     10.00000    48.00000     2.70000  # vert  10
     12.00000    48.00000     2.70000  # vert  11
     16.00000    48.00000     2.70000  # vert  12
     16.00000    36.00000     3.80000  # vert  13
     34.00000    36.00000     3.80000  # vert  14
     34.00000    45.50000     3.80000  # vert  15
     29.50000    45.50000     3.80000  # vert  16
     29.50000    48.00000     3.80000  # vert  17
     29.50000    50.50000     3.80000  # vert  18
     34.00000    50.50000     3.80000  # vert  19
     34.00000    60.00000     3.80000  # vert  20
     10.00000    60.00000     3.80000  # vert  21
     10.00000    48.00000     3.80000  # vert  22
     12.00000    48.00000     3.80000  # vert  23
     16.00000    48.00000     3.80000  # vert  24
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
   6, 13, 14, 15, 16, 17, 24,
   6,  1, 12,  5,  4,  3,  2,
   8, 21, 22, 23, 24, 17, 18, 19, 20,
   8, 10,  9,  8,  7,  6,  5, 12, 11,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, prt_g-h       OPAQ  VERT  gyp_blk_ptn  ceil_chi       
  2, east_s        OPAQ  VERT  insul_mtl_p  EXTERIOR       
  3, str_4         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  4, str_5         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  5, str_6         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  6, str_7         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  7, east_n        OPAQ  VERT  insul_mtl_p  EXTERIOR       
  8, north         OPAQ  VERT  insul_mtl_p  EXTERIOR       
  9, west          OPAQ  VERT  insul_mtl_p  EXTERIOR       
 10, cor_d         OPAQ  VERT  gyp_blk_ptn  ceil_chi       
 11, prt_t1        OPAQ  VERT  gyp_blk_ptn  toilets        
 12, prt_t2        OPAQ  VERT  gyp_blk_ptn  toilets        
 13, floor_b       OPAQ  CEIL  susp_flr_re  Unit_b         
 14, ceil_g        OPAQ  FLOR  susp_ceil    Unit_g         
 15, floor_a       OPAQ  CEIL  susp_flr_re  Unit_a         
 16, ceil_f        OPAQ  FLOR  susp_ceil    Unit_f         
# base
  0  0  0  0  0  0   481.50 1
