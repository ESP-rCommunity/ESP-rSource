# geometry of hall_up defined in: ../zone/hall_up.geo
GEN  hall_up  hall_up describes the upper portion of the hall and stairs  # type, name, descr
      20      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     2.70000  # vert   1
      2.30000     0.00000     2.70000  # vert   2
      2.30000     2.10000     2.70000  # vert   3
      3.30000     2.10000     2.70000  # vert   4
      3.30000     3.10000     2.70000  # vert   5
      0.00000     3.10000     2.70000  # vert   6
      0.00000     0.00000     5.10000  # vert   7
      2.30000     0.00000     5.10000  # vert   8
      2.30000     2.10000     5.10000  # vert   9
      3.30000     2.10000     5.10000  # vert  10
      3.30000     3.10000     5.10000  # vert  11
      0.00000     3.10000     5.10000  # vert  12
      2.30000     0.00000     4.92500  # vert  13
      0.00000     0.00000     4.92500  # vert  14
      2.10000     0.00000     2.70000  # vert  15
      2.10000     3.10000     2.70000  # vert  16
      1.00000     0.00000     3.40000  # vert  17
      2.10000     0.00000     3.40000  # vert  18
      2.10000     0.00000     4.80000  # vert  19
      1.00000     0.00000     4.80000  # vert  20
# no of vertices followed by list of associated vert
  11,  1, 15,  2, 13, 14,  1, 17, 20, 19, 18, 17,
   5,  2,  3,  9,  8, 13,
   4,  3,  4, 10,  9,
   4,  4,  5, 11, 10,
   5,  5, 16,  6, 12, 11,
   5,  6,  1, 14,  7, 12,
   6,  7,  8,  9, 10, 11, 12,
   4,  1,  6, 16, 15,
   6, 15, 16,  5,  4,  3,  2,
   4, 14, 13,  8,  7,
   4, 17, 18, 19, 20,
# unused index
  0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, hallup_ex_s   OPAQ  VERT  ext_wall_up  EXTERIOR       
  2, bed_1_r       OPAQ  VERT  partition    bed_1          
  3, bed_1_r2      OPAQ  VERT  partition    bed_1          
  4, bed_1_r3      OPAQ  VERT  partition    bed_1          
  5, bed_1         OPAQ  VERT  partition    bed_3          
  6, hallup_ex_w   OPAQ  VERT  ext_wall_up  EXTERIOR       
  7, ceil          OPAQ  CEIL  ceil_loft    loft           
  8, hall_r        OPAQ  FLOR  ceiling_rev  hall           
  9, liv_r         OPAQ  FLOR  ceiling_rev  living_room    
 10, loft          OPAQ  VERT  ext_wall_up  loft           
 11, s_glaz        TRAN  VERT  glazing      EXTERIOR       
# base
  8  9  0  0  0  0     8.13
