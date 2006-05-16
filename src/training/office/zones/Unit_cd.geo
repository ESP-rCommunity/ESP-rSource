# geometry of Unit_cd defined in: ../zones/Unit_cd.geo
GEN  Unit_cd  Unit_cd describes a...  # type, name, descr
      26      13   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     12.00000    12.00000     3.80000  # vert   1
     29.00000    12.00000     3.80000  # vert   2
     29.00000    14.50000     3.80000  # vert   3
     34.00000    14.50000     3.80000  # vert   4
     34.00000    36.00000     3.80000  # vert   5
     16.00000    36.00000     3.80000  # vert   6
     12.00000    36.00000     3.80000  # vert   7
     12.00000    12.00000     6.50000  # vert   8
     29.00000    12.00000     6.50000  # vert   9
     29.00000    14.50000     6.50000  # vert  10
     34.00000    14.50000     6.50000  # vert  11
     34.00000    36.00000     6.50000  # vert  12
     16.00000    36.00000     6.50000  # vert  13
     12.00000    36.00000     6.50000  # vert  14
     12.00000    28.00000     3.80000  # vert  15
     12.00000    20.00000     3.80000  # vert  16
     12.00000    20.00000     5.90000  # vert  17
     12.00000    28.00000     5.90000  # vert  18
     34.00000    15.50000     5.00000  # vert  19
     34.00000    30.50000     5.00000  # vert  20
     34.00000    30.50000     5.90000  # vert  21
     34.00000    15.50000     5.90000  # vert  22
     12.20000    24.00000     3.90000  # vert  23
     33.80000    24.00000     3.90000  # vert  24
     33.80000    24.00000     6.40000  # vert  25
     12.10000    24.00000     6.40000  # vert  26
# no of vertices followed by list of associated vert
   8,  7, 15, 18, 17, 16,  1,  8, 14,
   4,  1,  2,  9,  8,
   4,  2,  3, 10,  9,
   4,  3,  4, 11, 10,
  10,  4,  5, 12, 11,  4, 19, 22, 21, 20, 19,
   4,  5,  6, 13, 12,
   4,  6,  7, 14, 13,
   7,  8,  9, 10, 11, 12, 13, 14,
   9,  7,  6,  5,  4,  3,  2,  1, 16, 15,
   4, 15, 16, 17, 18,
   4, 19, 20, 21, 22,
   4, 23, 24, 25, 26,
   4, 24, 23, 26, 25,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, passg         OPAQ  VERT  mass_part    corid_1        
  2, prt_de        OPAQ  VERT  gyp_blk_ptn  Unit_e         
  3, str_2         OPAQ  VERT  gyp_blk_ptn  stair_deij     
  4, str_3         OPAQ  VERT  gyp_blk_ptn  stair_deij     
  5, east          OPAQ  VERT  insul_mtl_p  EXTERIOR       
  6, prt_bc        OPAQ  VERT  gyp_blk_ptn  Unit_b         
  7, prt_tc        OPAQ  VERT  gyp_blk_ptn  toilets        
  8, ceil          OPAQ  CEIL  susp_ceil    roof           
  9, floor         OPAQ  FLOR  susp_floor   ceil_chi       
 10, door          TRAN  VERT  dbl_glz      corid_1        
 11, w_glaz        TRAN  VERT  dbl_glz      EXTERIOR       
 12, inner_p1      OPAQ  VERT  mass_part    ADIABATIC      
 13, inner_p2      OPAQ  VERT  mass_part    ADIABATIC      
# base
  9  0  0  0  0  0   515.50 0
