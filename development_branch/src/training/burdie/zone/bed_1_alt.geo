# geometry of bed_1 defined in: ../zone/bed_1.geo
GEN  bed_1  bed_1 describes a south facing bedroom  # type, name, descr
      18      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      2.30000     0.00000     2.70000  # vert   1
      6.90000     0.00000     2.70000  # vert   2
      6.90000     3.10000     2.70000  # vert   3
      3.30000     3.10000     2.70000  # vert   4
      3.30000     2.10000     2.70000  # vert   5
      2.30000     2.10000     2.70000  # vert   6
      2.30000     0.00000     5.10000  # vert   7
      6.90000     0.00000     5.10000  # vert   8
      6.90000     3.10000     5.10000  # vert   9
      3.30000     3.10000     5.10000  # vert  10
      3.30000     2.10000     5.10000  # vert  11
      2.30000     2.10000     5.10000  # vert  12
      2.30000     0.00000     4.92500  # vert  13
      6.90000     0.00000     4.92500  # vert  14
      3.40000     0.00000     3.40000  # vert  15
      5.60000     0.00000     3.40000  # vert  16
      5.60000     0.00000     4.90000  # vert  17
      3.40000     0.00000     4.90000  # vert  18
# no of vertices followed by list of associated vert
  10,  1,  2, 14, 13,  1, 15, 18, 17, 16, 15,
   5,  2,  3,  9,  8, 14,
   4,  3,  4, 10,  9,
   4,  4,  5, 11, 10,
   4,  5,  6, 12, 11,
   5,  6,  1, 13,  7, 12,
   6,  7,  8,  9, 10, 11, 12,
   6,  6,  5,  4,  3,  2,  1,
   4, 13, 14,  8,  7,
   4, 15, 16, 17, 18,
# unused index
 0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   8   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, bed1_ex_s     OPAQ  VERT  ext_wall_up  EXTERIOR       
  2, bed1_sim_e    OPAQ  VERT  intern_wall  SIMILAR        
  3, bed_2_r       OPAQ  VERT  partition    ANOTHER        
  4, hallup_r_1    OPAQ  VERT  partition    ANOTHER        
  5, hallup_r_2    OPAQ  VERT  partition    ANOTHER        
  6, hallup_r_3    OPAQ  VERT  partition    ANOTHER        
  7, loft_r        OPAQ  CEIL  ceil_loft    ANOTHER        
  8, liv_r         OPAQ  FLOR  ceiling_rev  ANOTHER        
  9, loft_s_r      OPAQ  VERT  ext_wall_up  ANOTHER        
 10, s_glaz        TRAN  VERT  glazing      EXTERIOR       
# base
  8  0  0  0  0  0    13.26 0
