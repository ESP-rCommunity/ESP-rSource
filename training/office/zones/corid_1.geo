# geometry of corid_1 defined in: ../zones/corid_1.geo
GEN  corid_1  corid_1 describes a...  # type, name, descr
      26      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.00000    36.00000     3.80000  # vert   1
     10.00000    12.00000     3.80000  # vert   2
     12.00000    12.00000     3.80000  # vert   3
     12.00000    36.00000     3.80000  # vert   4
     12.00000    48.00000     3.80000  # vert   5
     10.00000    48.00000     3.80000  # vert   6
     10.00000    46.00000     3.80000  # vert   7
     10.00000    38.00000     3.80000  # vert   8
     10.00000    36.00000     6.50000  # vert   9
     10.00000    12.00000     6.50000  # vert  10
     12.00000    12.00000     6.50000  # vert  11
     12.00000    36.00000     6.50000  # vert  12
     12.00000    48.00000     6.50000  # vert  13
     10.00000    48.00000     6.50000  # vert  14
     10.00000    46.00000     6.50000  # vert  15
     10.00000    38.00000     6.50000  # vert  16
      7.28300    42.13900     3.80000  # vert  17
      7.28300    42.13900     6.50000  # vert  18
     10.00000    35.50000     4.53000  # vert  19
     10.00000    12.50000     4.53000  # vert  20
     10.00000    12.50000     6.03000  # vert  21
     10.00000    35.50000     6.03000  # vert  22
     12.00000    20.00000     3.80000  # vert  23
     12.00000    28.00000     3.80000  # vert  24
     12.00000    28.00000     5.90000  # vert  25
     12.00000    20.00000     5.90000  # vert  26
# no of vertices followed by list of associated vert
  10,  1,  2, 10,  9,  1, 19, 22, 21, 20, 19,
   4,  2,  3, 11, 10,
   8,  3, 23, 26, 25, 24,  4, 12, 11,
   4,  4,  5, 13, 12,
   4,  5,  6, 14, 13,
   4,  6,  7, 15, 14,
   4,  7, 17, 18, 15,
   4,  8,  1,  9, 16,
   9,  9, 10, 11, 12, 13, 14, 15, 18, 16,
  11,  1,  8, 17,  7,  6,  5,  4, 24, 23,  3,  2,
   4, 17,  8, 16, 18,
   4, 19, 20, 21, 22,
   4, 23, 24, 25, 26,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, west_cor      OPAQ  VERT  insul_mtl_p  EXTERIOR       
  2, door          OPAQ  VERT  door         Unit_e         
  3, prt_d-cor     OPAQ  VERT  mass_part    Unit_cd        
  4, prt_t_cor     OPAQ  VERT  gyp_blk_ptn  toilets        
  5, door          OPAQ  VERT  door         Unit_a         
  6, ent_a         TRAN  VERT  dbl_glz      EXTERIOR       
  7, ent_b         TRAN  VERT  dbl_glz      EXTERIOR       
  8, ent_c         TRAN  VERT  dbl_glz      EXTERIOR       
  9, ceil          OPAQ  CEIL  susp_ceil    roof           
 10, floor         OPAQ  FLOR  susp_floor   ceil_chi       
 11, ent_d         TRAN  VERT  dbl_glz      EXTERIOR       
 12, glz_w         TRAN  VERT  dbl_glz      EXTERIOR       
 13, door_dcor     TRAN  VERT  dbl_glz      Unit_cd        
# base
 10  0  0  0  0  0    82.87 0
