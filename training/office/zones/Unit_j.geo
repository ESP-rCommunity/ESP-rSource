# geometry of Unit_j defined in: ../zones/Unit_j.geo
GEN  Unit_j  Unit_j describes a...  # type, name, descr
      26      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.00000     0.00000     0.00000  # vert   1
     34.00000     0.00000     0.00000  # vert   2
     34.00000     9.50000     0.00000  # vert   3
     29.00000     9.50000     0.00000  # vert   4
     29.00000    12.00000     0.00000  # vert   5
     12.00000    12.00000     0.00000  # vert   6
     10.00000    12.00000     0.00000  # vert   7
     10.00000     0.00000     2.70000  # vert   8
     34.00000     0.00000     2.70000  # vert   9
     34.00000     9.50000     2.70000  # vert  10
     29.00000     9.50000     2.70000  # vert  11
     29.00000    12.00000     2.70000  # vert  12
     12.00000    12.00000     2.70000  # vert  13
     10.00000    12.00000     2.70000  # vert  14
     34.00000     0.50000     1.20000  # vert  15
     34.00000     9.00000     1.20000  # vert  16
     34.00000     9.00000     2.10000  # vert  17
     34.00000     0.50000     2.10000  # vert  18
     10.50000     0.00000     1.20000  # vert  19
     33.50000     0.00000     1.20000  # vert  20
     33.50000     0.00000     2.10000  # vert  21
     10.50000     0.00000     2.10000  # vert  22
     10.00000    11.50000     0.73000  # vert  23
     10.00000     0.50000     0.73000  # vert  24
     10.00000     0.50000     2.23000  # vert  25
     10.00000    11.50000     2.23000  # vert  26
# no of vertices followed by list of associated vert
  10,  1,  2,  9,  8,  1, 19, 22, 21, 20, 19,
  10,  2,  3, 10,  9,  2, 15, 18, 17, 16, 15,
   4,  3,  4, 11, 10,
   4,  4,  5, 12, 11,
   4,  5,  6, 13, 12,
   4,  6,  7, 14, 13,
  10,  7,  1,  8, 14,  7, 23, 26, 25, 24, 23,
   7,  8,  9, 10, 11, 12, 13, 14,
   7,  1,  7,  6,  5,  4,  3,  2,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
   4, 23, 24, 25, 26,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  insul_mtl_p  EXTERIOR       
  2, east          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  3, str_3         OPAQ  VERT  gyp_blk_ptn  stair_deij     
  4, str_4         OPAQ  VERT  gyp_blk_ptn  stair_deij     
  5, prt_i-j       OPAQ  VERT  gyp_blk_ptn  Unit_hi        
  6, door          OPAQ  VERT  door         corid_g        
  7, west          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  8, ceil_j        OPAQ  CEIL  susp_ceil    ceil_j         
  9, floor         OPAQ  FLOR  grnd_floor   GROUND         
 10, glz_e         TRAN  VERT  dbl_glz      EXTERIOR       
 11, glz_s         TRAN  VERT  dbl_glz      EXTERIOR       
 12, glz_w         TRAN  VERT  dbl_glz      EXTERIOR       
# base
  9  0  0  0  0  0   275.50 0
