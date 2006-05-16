# geometry of bed_3_air_g defined in: ../zone/bed_3_air_gap.geo
GEN  bed_3_air_g  bed_3_air_g is the air gap on north face of bedroom 3  # type, name, descr
      18      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     6.33600     2.70000  # vert   1
      0.00000     6.40000     2.70000  # vert   2
      3.30000     6.33600     2.70000  # vert   3
      0.00000     6.33600     4.92500  # vert   4
      3.30000     6.33600     4.92500  # vert   5
      2.10000     6.33600     3.40000  # vert   6
      2.10000     6.33600     4.90000  # vert   7
      1.10000     6.33600     4.90000  # vert   8
      1.10000     6.33600     3.40000  # vert   9
      3.30000     6.40000     2.70000  # vert  10
      2.10000     6.40000     3.40000  # vert  11
      1.10000     6.40000     3.40000  # vert  12
      1.10000     6.40000     4.90000  # vert  13
      2.10000     6.40000     4.90000  # vert  14
      3.30000     6.40000     4.92500  # vert  15
      0.00000     6.40000     4.92500  # vert  16
      2.10000     6.33600     2.70000  # vert  17
      2.10000     6.40000     2.70000  # vert  18
# no of vertices followed by list of associated vert
   4, 17, 18, 10,  3,
  11,  3,  6,  9,  8,  7,  6,  3,  5,  4,  1, 17,
  11, 10, 11, 14, 13, 12, 11, 10, 18,  2, 16, 15,
   4, 16,  4,  5, 15,
   4,  7,  8, 13, 14,
   4,  2,  1,  4, 16,
   4,  6, 11, 12,  9,
   4, 11,  6,  7, 14,
   4, 13,  8,  9, 12,
   4, 15,  5,  3, 10,
   4, 17,  1,  2, 18,
# unused index
  0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, first         OPAQ  FLOR  ceiling_rev  kitchen        
  2, xbed3_ex_n    OPAQ  VERT  air_gap_intr bed_3          
  3, xxbed3_ex_n   OPAQ  VERT  air_gap_ext  EXTERIOR       
  4, xbedroom      OPAQ  CEIL  steel_sheet  t_bridge       
  5, Surf-5        OPAQ  FLOR  steel_sheet  EXTERIOR       
  6, gap_ext_w     OPAQ  VERT  ext_wall_up  EXTERIOR       
  7, Surf-7        OPAQ  CEIL  steel_sheet  EXTERIOR       
  8, Surf-8        OPAQ  VERT  steel_sheet  EXTERIOR       
  9, Surf-9        OPAQ  VERT  steel_sheet  EXTERIOR       
 10, Surf-10       OPAQ  VERT  partition    bed_2          
 11, xair_gap_r    OPAQ  FLOR  ceiling_rev  bathroom       
# base
  1  5 11  0  0  0     0.28
