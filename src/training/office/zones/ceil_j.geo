# geometry of ceil_j defined in: ../zones/ceil_j.geo
GEN  ceil_j  ceil_j describes a...  # type, name, descr
      14       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.00000     0.00000     2.70000  # vert   1
     34.00000     0.00000     2.70000  # vert   2
     34.00000     9.50000     2.70000  # vert   3
     29.00000     9.50000     2.70000  # vert   4
     29.00000    12.00000     2.70000  # vert   5
     12.00000    12.00000     2.70000  # vert   6
      9.99900    12.00000     2.70000  # vert   7
     10.00000     0.00000     3.80000  # vert   8
     34.00000     0.00000     3.80000  # vert   9
     34.00000     9.50000     3.80000  # vert  10
     29.00000     9.50000     3.80000  # vert  11
     29.00000    12.00000     3.80000  # vert  12
     12.00000    12.00000     3.80000  # vert  13
     10.00000    12.00000     3.80000  # vert  14
# no of vertices followed by list of associated vert
   4,  1,  2,  9,  8,
   4,  2,  3, 10,  9,
   4,  3,  4, 11, 10,
   4,  4,  5, 12, 11,
   4,  5,  6, 13, 12,
   4,  6,  7, 14, 13,
   4,  7,  1,  8, 14,
   7,  8,  9, 10, 11, 12, 13, 14,
   7,  1,  7,  6,  5,  4,  3,  2,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, south         OPAQ  VERT  insul_mtl_p  EXTERIOR       
  2, east          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  3, str_3         OPAQ  VERT  gyp_blk_ptn  stair_deij     
  4, str_4         OPAQ  VERT  gyp_blk_ptn  stair_deij     
  5, prt_i-j       OPAQ  VERT  gyp_blk_ptn  ceil_chi       
  6, cor_dj        OPAQ  VERT  gyp_blk_ptn  ceil_chi       
  7, west          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  8, upper         OPAQ  CEIL  susp_flr_re  Unit_e         
  9, lower         OPAQ  FLOR  susp_ceil    Unit_j         
# base
  9  0  0  0  0  0   275.51 0
