# geometry of bed_2 defined in: ../zone/bed_2.geo
GEN  bed_2  bed_2 describes a north facing bedroom  # type, name, descr
      21      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      3.30000     3.10000     2.70000  # vert   1
      6.90000     3.10000     2.70000  # vert   2
      6.90000     6.40000     2.70000  # vert   3
      3.30000     6.40000     2.70000  # vert   4
      3.30000     3.10000     5.10000  # vert   5
      6.90000     3.10000     5.10000  # vert   6
      6.90000     6.40000     5.10000  # vert   7
      3.30000     6.40000     5.10000  # vert   8
      6.90000     6.40000     4.92500  # vert   9
      3.30000     6.40000     4.92500  # vert  10
      5.20000     3.70000     2.70000  # vert  11
      6.90000     3.70000     2.70000  # vert  12
      5.20000     6.40000     2.70000  # vert  13
      3.30000     3.70000     2.70000  # vert  14
      5.60000     6.40000     3.40000  # vert  15
      3.40000     6.40000     3.40000  # vert  16
      3.40000     6.40000     4.90000  # vert  17
      5.60000     6.40000     4.90000  # vert  18
      3.30000     6.33600     4.92500  # vert  19
      3.30000     6.33600     2.70000  # vert  20
      3.30000     6.33600     5.10000  # vert  21
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   6,  2, 12,  3,  9,  7,  6,
  11,  3, 13,  4, 10,  9,  3, 15, 18, 17, 16, 15,
   6, 20, 14,  1,  5, 21, 19,
   5,  5,  6,  7,  8, 21,
   5, 14, 11, 12,  2,  1,
   4,  7,  9, 10,  8,
   4, 12, 11, 13,  3,
   5, 11, 14, 20,  4, 13,
   4, 15, 16, 17, 18,
   4, 19, 10,  4, 20,
   4, 10, 19, 21,  8,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, bed1_r        OPAQ  VERT  partition    bed_1          
  2, bed2_sim_e    OPAQ  VERT  intern_wall  SIMILAR        
  3, bed2_ex_n     OPAQ  VERT  ext_wall_up  EXTERIOR       
  4, bed_3_r       OPAQ  VERT  partition    bed_3          
  5, loft_r        OPAQ  CEIL  ceil_loft    loft           
  6, liv_r         OPAQ  FLOR  ceiling_rev  living_room    
  7, loft_n_r      OPAQ  VERT  ext_wall_up  loft           
  8, store_r       OPAQ  FLOR  ceiling_rev  store          
  9, kitch_r       OPAQ  FLOR  ceiling_rev  kitchen        
 10, n_glaz        TRAN  VERT  glazing      EXTERIOR       
 11, xSurf-10      OPAQ  VERT  partition    bed_3_air_gap  
 12, xbed_2_r      OPAQ  VERT  partition    t_bridge       
# base
  6  8  9  0  0  0    11.88
