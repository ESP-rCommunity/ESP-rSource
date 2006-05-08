# geometry of Unit_b defined in: ../zones/Unit_b.geo
GEN  Unit_b  Unit_b describes a...  # type, name, descr
      16       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     16.00000    36.00000     3.80000  # vert   1
     34.00000    36.00000     3.80000  # vert   2
     34.00000    45.50000     3.80000  # vert   3
     29.50000    45.50000     3.80000  # vert   4
     29.50000    48.00000     3.80000  # vert   5
     16.00000    48.00000     3.80000  # vert   6
     16.00000    36.00000     6.50000  # vert   7
     34.00000    36.00000     6.50000  # vert   8
     34.00000    45.50000     6.50000  # vert   9
     29.50000    45.50000     6.50000  # vert  10
     29.50000    48.00000     6.50000  # vert  11
     16.00000    48.00000     6.50000  # vert  12
     34.00000    36.50000     5.00000  # vert  13
     34.00000    44.50000     5.00000  # vert  14
     34.00000    44.50000     5.90000  # vert  15
     34.00000    36.50000     5.90000  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2,  8,  7,
  10,  2,  3,  9,  8,  2, 13, 16, 15, 14, 13,
   4,  3,  4, 10,  9,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
   4,  6,  1,  7, 12,
   6,  7,  8,  9, 10, 11, 12,
   6,  1,  6,  5,  4,  3,  2,
   4, 13, 14, 15, 16,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, prt_b-c       OPAQ  VERT  gyp_blk_ptn  Unit_cd        
  2, east          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  3, str_4         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  4, str_5         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  5, prt_a-b       OPAQ  VERT  gyp_blk_ptn  Unit_a         
  6, prt_t_b       OPAQ  VERT  gyp_blk_ptn  toilets        
  7, ceil_b        OPAQ  CEIL  susp_ceil    roof           
  8, floor         OPAQ  FLOR  susp_floor   ceil_fg        
  9, glz_e         TRAN  VERT  dbl_glz      EXTERIOR       
# base
  8  0  0  0  0  0   204.75 0
