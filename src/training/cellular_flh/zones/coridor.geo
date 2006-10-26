# geometry of coridor defined in: ../zones/coridor.geo
GEN  coridor  coridor describes passage adjacent to offices  # type, name, descr
      32      14   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     4.55000     0.00000  # vert   1
      6.10000     4.55000     0.00000  # vert   2
      6.10000     5.55000     0.00000  # vert   3
      0.00000     5.55000     0.00000  # vert   4
      0.00000     4.55000     3.00000  # vert   5
      6.10000     4.55000     3.00000  # vert   6
      6.10000     5.55000     3.00000  # vert   7
      0.00000     5.55000     3.00000  # vert   8
      1.20000     4.55000     0.00000  # vert   9
      0.40000     4.55000     0.00000  # vert  10
      0.40000     4.55000     2.90000  # vert  11
      1.20000     4.55000     2.90000  # vert  12
      0.40000     4.55000     3.00000  # vert  13
      1.20000     4.55000     3.00000  # vert  14
      3.00000     4.55000     0.00000  # vert  15
      3.00000     4.55000     3.00000  # vert  16
      2.90000     4.55000     0.10000  # vert  17
      2.90000     4.55000     2.90000  # vert  18
      1.30000     4.55000     2.90000  # vert  19
      1.30000     4.55000     0.10000  # vert  20
      4.90000     4.55000     0.00000  # vert  21
      3.10000     4.55000     0.00000  # vert  22
      3.20000     4.55000     0.10000  # vert  23
      4.80000     4.55000     0.10000  # vert  24
      4.80000     4.55000     2.90000  # vert  25
      3.20000     4.55000     2.90000  # vert  26
      3.10000     4.55000     3.00000  # vert  27
      4.90000     4.55000     3.00000  # vert  28
      4.90000     4.55000     2.90000  # vert  29
      5.70000     4.55000     0.00000  # vert  30
      5.70000     4.55000     2.90000  # vert  31
      5.70000     4.55000     3.00000  # vert  32
# no of vertices followed by list of associated vert
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
  10,  5, 13, 14, 16, 27, 28, 32,  6,  7,  8,
  10,  1,  4,  3,  2, 30, 21, 22, 15,  9, 10,
   4, 10,  9, 12, 11,
   7,  1, 10, 11, 12, 14, 13,  5,
  11,  9, 15, 17, 20, 19, 18, 17, 15, 16, 14, 12,
   4, 20, 17, 18, 19,
  11, 22, 21, 29, 28, 27, 22, 23, 26, 25, 24, 23,
   4, 21, 30, 31, 29,
   7, 30,  2,  6, 32, 28, 29, 31,
   4, 23, 24, 25, 26,
   4, 15, 22, 27, 16,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, right         OPAQ  VERT  door         SIMILAR        
  2, wall          OPAQ  VERT  gyp_blk_ptn  SIMILAR        
  3, left          OPAQ  VERT  door         SIMILAR        
  4, ceiling       OPAQ  CEIL  ceiling_scrd ceiling_slb    
  5, floor         OPAQ  FLOR  screed       floor          
  6, door          OPAQ  VERT  door         manager_a      
  7, ptn_corid     OPAQ  VERT  gyp_gyp_ptn  manager_a      
  8, part_frame    OPAQ  VERT  insul_frame  manager_a      
  9, part_glaz     TRAN  VERT  dbl_glz      manager_a      
 10, part_frameb   OPAQ  VERT  insul_frame  manager_b      
 11, door_b        OPAQ  VERT  door         manager_b      
 12, ptn_coridb    OPAQ  VERT  gyp_gyp_ptn  manager_b      
 13, part_glazb    TRAN  VERT  dbl_glz      manager_b      
 14, filler        OPAQ  VERT  gyp_gyp_ptn  ADIABATIC      
# base
  6  5  0  0  0  0     8.42 0
