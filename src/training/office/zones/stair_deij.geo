# geometry of stair_deij defined in: ../zones/stair_deij.geo
GEN  stair_deij  stair_deij describes a...  # type, name, descr
      24      16   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     34.00000     9.50000     0.00000  # vert   1
     34.00000    14.50000     0.00000  # vert   2
     29.00000    14.50000     0.00000  # vert   3
     29.00000    12.00000     0.00000  # vert   4
     29.00000     9.50000     0.00000  # vert   5
     34.00000     9.50000     6.50000  # vert   6
     34.00000    14.50000     6.50000  # vert   7
     29.00000    14.50000     6.50000  # vert   8
     29.00000    12.00000     6.50000  # vert   9
     29.00000     9.50000     6.50000  # vert  10
     34.00000     9.50000     2.70000  # vert  11
     34.00000     9.50000     3.80000  # vert  12
     34.00000    14.50000     2.70000  # vert  13
     34.00000    14.50000     3.80000  # vert  14
     29.00000    14.50000     2.70000  # vert  15
     29.00000    14.50000     3.80000  # vert  16
     29.00000    12.00000     2.70000  # vert  17
     29.00000    12.00000     3.80000  # vert  18
     29.00000     9.50000     2.70000  # vert  19
     29.00000     9.50000     3.80000  # vert  20
     34.00000    10.50000     0.00000  # vert  21
     34.00000    11.30000     0.00000  # vert  22
     34.00000    11.30000     2.10000  # vert  23
     34.00000    10.50000     2.10000  # vert  24
# no of vertices followed by list of associated vert
  12,  1, 21, 24, 23, 22,  2, 13, 14,  7,  6, 12, 11,
   4,  2,  3, 15, 13,
   4,  3,  4, 17, 15,
   4,  4,  5, 19, 17,
   4,  5,  1, 11, 19,
   5,  6,  7,  8,  9, 10,
   7,  1,  5,  4,  3,  2, 22, 21,
   4, 19, 11, 12, 20,
   4, 20, 12,  6, 10,
   4, 17, 19, 20, 18,
   4, 18, 20, 10,  9,
   4, 15, 17, 18, 16,
   4, 16, 18,  9,  8,
   4, 13, 15, 16, 14,
   4, 14, 16,  8,  7,
   4, 21, 22, 23, 24,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, east          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  2, prt_i         OPAQ  VERT  gyp_blk_ptn  Unit_hi        
  3, prt_i1        OPAQ  VERT  gyp_blk_ptn  Unit_hi        
  4, prt_j         OPAQ  VERT  gyp_blk_ptn  Unit_j         
  5, prt_j1        OPAQ  VERT  gyp_blk_ptn  Unit_j         
  6, ceil          OPAQ  CEIL  susp_ceil    roof           
  7, floor         OPAQ  FLOR  grnd_floor   GROUND         
  8, prt_cj1       OPAQ  VERT  gyp_blk_ptn  ceil_j         
  9, prt_e1        OPAQ  VERT  gyp_blk_ptn  Unit_e         
 10, prt_cj        OPAQ  VERT  gyp_blk_ptn  ceil_j         
 11, prt_e         OPAQ  VERT  gyp_blk_ptn  Unit_e         
 12, prt_ci1       OPAQ  VERT  gyp_blk_ptn  ceil_chi       
 13, prt_d1        OPAQ  VERT  gyp_blk_ptn  Unit_cd        
 14, prt_ci        OPAQ  VERT  gyp_blk_ptn  ceil_chi       
 15, prt_d         OPAQ  VERT  gyp_blk_ptn  Unit_cd        
 16, door          OPAQ  VERT  door         EXTERIOR       
# base
  7  0  0  0  0  0    25.00 0
