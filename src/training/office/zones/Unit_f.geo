# geometry of Unit_f defined in: ../zones/Unit_f.geo
GEN  Unit_f  Unit_f describes a...  # type, name, descr
      28      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     12.00000    48.00000     0.00000  # vert   1
     16.00000    48.00000     0.00000  # vert   2
     29.50000    48.00000     0.00000  # vert   3
     29.50000    50.50000     0.00000  # vert   4
     34.00000    50.50000     0.00000  # vert   5
     34.00000    60.00000     0.00000  # vert   6
     10.00000    60.00000     0.00000  # vert   7
     12.00000    48.00000     2.70000  # vert   8
     16.00000    48.00000     2.70000  # vert   9
     29.50000    48.00000     2.70000  # vert  10
     29.50000    50.50000     2.70000  # vert  11
     34.00000    50.50000     2.70000  # vert  12
     34.00000    60.00000     2.70000  # vert  13
     10.00000    60.00000     2.70000  # vert  14
     10.00000    48.00000     0.00000  # vert  15
     10.00000    48.00000     2.70000  # vert  16
     34.00000    51.00000     1.20000  # vert  17
     34.00000    59.00000     1.20000  # vert  18
     34.00000    59.00000     2.10000  # vert  19
     34.00000    51.00000     2.10000  # vert  20
     33.50000    60.00000     1.20000  # vert  21
     10.50000    60.00000     1.20000  # vert  22
     10.50000    60.00000     2.10000  # vert  23
     33.50000    60.00000     2.10000  # vert  24
     10.00000    59.50000     0.73000  # vert  25
     10.00000    48.50000     0.73000  # vert  26
     10.00000    48.50000     2.23000  # vert  27
     10.00000    59.50000     2.23000  # vert  28
# no of vertices followed by list of associated vert
   4,  1,  2,  9,  8,
   4,  2,  3, 10,  9,
   4,  3,  4, 11, 10,
   4,  4,  5, 12, 11,
  10,  5,  6, 13, 12,  5, 17, 20, 19, 18, 17,
  10,  6,  7, 14, 13,  6, 21, 24, 23, 22, 21,
  10,  7, 15, 16, 14,  7, 25, 28, 27, 26, 25,
   8, 16,  8,  9, 10, 11, 12, 13, 14,
   8,  1, 15,  7,  6,  5,  4,  3,  2,
   4, 15,  1,  8, 16,
   4, 17, 18, 19, 20,
   4, 21, 22, 23, 24,
   4, 25, 26, 27, 28,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, prt_f-t       OPAQ  VERT  gyp_blk_ptn  toilets        
  2, prt_f-g       OPAQ  VERT  gyp_blk_ptn  Unit_g         
  3, str_3         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  4, str_4         OPAQ  VERT  gyp_blk_ptn  stair_abfg     
  5, east          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  6, north         OPAQ  VERT  insul_mtl_p  EXTERIOR       
  7, west          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  8, ceil_f        OPAQ  CEIL  susp_ceil    ceil_fg        
  9, floor         OPAQ  FLOR  grnd_floor   GROUND         
 10, door          OPAQ  VERT  door         corid_g        
 11, glz_e         TRAN  VERT  dbl_glz      EXTERIOR       
 12, glz_n         TRAN  VERT  dbl_glz      EXTERIOR       
 13, glz_w         TRAN  VERT  dbl_glz      EXTERIOR       
# base
  9  0  0  0  0  0   276.75 0
