# geometry of ceil_void defined in: ../zones/ceil_void.geo
GEN  ceil_void  ceil_void is a 0.5m ceiling void over offices  # type, name, descr
      33      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     3.00000  # vert   1
      3.00000     0.00000     3.00000  # vert   2
      3.00000     4.50000     3.00000  # vert   3
      1.00000     4.50000     3.00000  # vert   4
      0.00000     4.50000     3.00000  # vert   5
      6.00000     0.00000     3.00000  # vert   6
      9.00000     0.00000     3.00000  # vert   7
     12.00000     0.00000     3.00000  # vert   8
     12.00000     3.00000     3.00000  # vert   9
     12.00000     6.00000     3.00000  # vert  10
      9.00000     6.00000     3.00000  # vert  11
      3.00000     6.00000     3.00000  # vert  12
      0.00000     6.00000     3.00000  # vert  13
      6.00000    13.50000     3.00000  # vert  14
      9.00000    12.00000     3.00000  # vert  15
     12.00000    12.00000     3.00000  # vert  16
     12.00000    15.00000     3.00000  # vert  17
     12.00000    18.00000     3.00000  # vert  18
      9.00000    18.00000     3.00000  # vert  19
      6.00000    18.00000     3.00000  # vert  20
      3.00000    18.00000     3.00000  # vert  21
      3.00000    13.50000     3.00000  # vert  22
     12.00000     9.00000     3.00000  # vert  23
      0.00000     0.00000     3.50000  # vert  24
      3.00000     0.00000     3.50000  # vert  25
     12.00000     0.00000     3.50000  # vert  26
     12.00000     6.00000     3.50000  # vert  27
     12.00000    12.00000     3.50000  # vert  28
     12.00000    18.00000     3.50000  # vert  29
      3.00000    18.00000     3.50000  # vert  30
      3.00000    13.50000     3.50000  # vert  31
      3.00000     6.00000     3.50000  # vert  32
      0.00000     6.00000     3.50000  # vert  33
# no of vertices followed by list of associated vert
   5,  2,  1,  5,  4,  3,
  12,  4,  5, 13, 12, 11, 10,  9,  8,  7,  6,  2,  3,
   9, 15, 14, 22, 21, 20, 19, 18, 17, 16,
   8, 11, 12, 22, 14, 15, 16, 23, 10,
  10, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
   8,  1,  2,  6,  7,  8, 26, 25, 24,
  11,  8,  9, 10, 23, 16, 17, 18, 29, 28, 27, 26,
   6, 18, 19, 20, 21, 30, 29,
   6, 21, 22, 12, 32, 31, 30,
   4, 12, 13, 33, 32,
   5, 13,  5,  1, 24, 33,
# unused index
  0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-6        OPAQ  FLOR  ceiling_rev  manager        
  2, ceiling       OPAQ  FLOR  ceiling_rev  general        
  3, ceil_conff    OPAQ  FLOR  ceiling_rev  conference     
  4, ceil_rec      OPAQ  FLOR  ceiling_rev  reception      
  5, structure     OPAQ  CEIL  susp_floor   SIMILAR        
  6, south_edge    OPAQ  VERT  extern_wall  EXTERIOR       
  7, east_edge     OPAQ  VERT  extern_wall  EXTERIOR       
  8, north_edge    OPAQ  VERT  extern_wall  EXTERIOR       
  9, core_b        OPAQ  VERT  gyp_blk_ptn  SIMILAR        
 10, core_a        OPAQ  VERT  gyp_blk_ptn  mixing_box     
 11, core_c        OPAQ  VERT  gyp_blk_ptn  SIMILAR        
# base
  1  2  3  4  0  0   180.00
