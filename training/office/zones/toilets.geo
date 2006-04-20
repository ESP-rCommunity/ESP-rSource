# geometry of toilets defined in: ../zones/toilets.geo
GEN  toilets  toilets describes a...  # type, name, descr
      16      14   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     12.00000    48.00000     0.00000  # vert   1
     12.00000    36.00000     0.00000  # vert   2
     16.00000    36.00000     0.00000  # vert   3
     16.00000    48.00000     0.00000  # vert   4
     12.00000    48.00000     6.50000  # vert   5
     12.00000    36.00000     6.50000  # vert   6
     16.00000    36.00000     6.50000  # vert   7
     16.00000    48.00000     6.50000  # vert   8
     12.00000    48.00000     2.70000  # vert   9
     12.00000    48.00000     3.80000  # vert  10
     12.00000    36.00000     2.70000  # vert  11
     12.00000    36.00000     3.80000  # vert  12
     16.00000    36.00000     2.70000  # vert  13
     16.00000    36.00000     3.80000  # vert  14
     16.00000    48.00000     2.70000  # vert  15
     16.00000    48.00000     3.80000  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2, 11,  9,
   4,  2,  3, 13, 11,
   4,  3,  4, 15, 13,
   4,  4,  1,  9, 15,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 11, 12, 10,
   4, 10, 12,  6,  5,
   4, 11, 13, 14, 12,
   4, 12, 14,  7,  6,
   4, 13, 15, 16, 14,
   4, 14, 16,  8,  7,
   4, 15,  9, 10, 16,
   4, 16, 10,  5,  8,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, prt_tcor      OPAQ  VERT  gyp_blk_ptn  corid_g        
  2, prt_th        OPAQ  VERT  gyp_blk_ptn  Unit_hi        
  3, prt_tg        OPAQ  VERT  gyp_blk_ptn  Unit_g         
  4, prt_ft        OPAQ  VERT  gyp_blk_ptn  Unit_f         
  5, ceiling       OPAQ  CEIL  susp_ceil    roof           
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, prt_tcorc     OPAQ  VERT  gyp_blk_ptn  ceil_chi       
  8, prt_tcor1     OPAQ  VERT  gyp_blk_ptn  corid_1        
  9, prt_cth       OPAQ  VERT  gyp_blk_ptn  ceil_chi       
 10, prt_ctc       OPAQ  VERT  gyp_blk_ptn  Unit_cd        
 11, prt_ctb       OPAQ  VERT  gyp_blk_ptn  ceil_fg        
 12, prt_tb        OPAQ  VERT  gyp_blk_ptn  Unit_b         
 13, prt_cat       OPAQ  VERT  gyp_blk_ptn  ceil_fg        
 14, prt_ta        OPAQ  VERT  gyp_blk_ptn  Unit_a         
# base
  6  0  0  0  0  0    48.00 0
