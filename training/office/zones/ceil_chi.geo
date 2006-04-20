# geometry of ceil_chi defined in: ../zones/ceil_chi.geo
GEN  ceil_chi  ceil_chi describes a...  # type, name, descr
      28      18   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.00000    48.00000     2.70000  # vert   1
     10.00000    46.00000     2.70000  # vert   2
      7.28300    42.13900     2.70000  # vert   3
     10.00000    38.00000     2.70000  # vert   4
     10.00000    36.00000     2.70000  # vert   5
     10.00000    12.00000     2.70000  # vert   6
     12.00000    12.00000     2.70000  # vert   7
     29.00000    12.00000     2.70000  # vert   8
     29.00000    14.50000     2.70000  # vert   9
     34.00000    14.50000     2.70000  # vert  10
     34.00000    36.00000     2.70000  # vert  11
     16.00000    36.00000     2.70000  # vert  12
     12.00000    36.00000     2.70000  # vert  13
     12.00000    48.00000     2.70000  # vert  14
     10.00000    48.00000     3.80000  # vert  15
     10.00000    46.00000     3.80000  # vert  16
      7.28300    42.13900     3.80000  # vert  17
     10.00000    38.00000     3.80000  # vert  18
     10.00000    36.00000     3.80000  # vert  19
     10.00000    12.00000     3.80000  # vert  20
     12.00000    12.00000     3.80000  # vert  21
     29.00000    12.00000     3.80000  # vert  22
     29.00000    14.50000     3.80000  # vert  23
     34.00000    14.50000     3.80000  # vert  24
     34.00000    36.00000     3.80000  # vert  25
     16.00000    36.00000     3.80000  # vert  26
     12.00000    36.00000     3.80000  # vert  27
     12.00000    48.00000     3.80000  # vert  28
# no of vertices followed by list of associated vert
   4,  1,  2, 16, 15,
   4,  2,  3, 17, 16,
   4,  3,  4, 18, 17,
   4,  4,  5, 19, 18,
   4,  5,  6, 20, 19,
   4,  6,  7, 21, 20,
   4,  7,  8, 22, 21,
   4,  8,  9, 23, 22,
   4,  9, 10, 24, 23,
   4, 10, 11, 25, 24,
   4, 11, 12, 26, 25,
   4, 12, 13, 27, 26,
   4, 13, 14, 28, 27,
   4, 14,  1, 15, 28,
   7, 21, 22, 23, 24, 25, 26, 27,
   7, 13, 12, 11, 10,  9,  8,  7,
   9,  6,  5,  4,  3,  2,  1, 14, 13,  7,
   9, 15, 16, 17, 18, 19, 20, 21, 27, 28,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, nw_ext        OPAQ  VERT  insul_mtl_p  EXTERIOR       
  2, ent_a         TRAN  VERT  dbl_glz      EXTERIOR       
  3, ent_b         TRAN  VERT  dbl_glz      EXTERIOR       
  4, ent_c         TRAN  VERT  dbl_glz      EXTERIOR       
  5, west          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  6, cor_j         OPAQ  VERT  gyp_blk_ptn  ceil_j         
  7, prt_ij        OPAQ  VERT  gyp_blk_ptn  ceil_j         
  8, str_2         OPAQ  VERT  gyp_blk_ptn  stair_deij     
  9, str_3         OPAQ  VERT  gyp_blk_ptn  stair_deij     
 10, east          OPAQ  VERT  insul_mtl_p  EXTERIOR       
 11, prt_gh        OPAQ  VERT  gyp_blk_ptn  ceil_fg        
 12, prt_th        OPAQ  VERT  gyp_blk_ptn  toilets        
 13, prt_tcor      OPAQ  VERT  gyp_blk_ptn  toilets        
 14, prt_fcor      OPAQ  VERT  gyp_blk_ptn  ceil_fg        
 15, upper         OPAQ  CEIL  susp_flr_re  Unit_cd        
 16, ceil          OPAQ  FLOR  susp_ceil    Unit_hi        
 17, cor_ceil      OPAQ  FLOR  susp_ceil    corid_g        
 18, up_cor        OPAQ  CEIL  susp_flr_re  corid_1        
# base
 16 17  0  0  0  0   598.37 0
