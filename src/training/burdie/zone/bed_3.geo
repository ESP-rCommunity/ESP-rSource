# geometry of bed_3 defined in: ../zone/bed_3.geo
GEN  bed_3  bed_3 describes room with moisture details  # type, name, descr
      24      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     3.10000     2.70000  # vert   1
      3.30000     3.10000     2.70000  # vert   2
      3.30000     6.33200     2.70000  # vert   3
      0.00000     6.33200     2.70000  # vert   4
      0.00000     3.10000     5.10000  # vert   5
      3.30000     3.10000     5.10000  # vert   6
      3.30000     6.33200     5.10000  # vert   7
      0.00000     6.33200     5.10000  # vert   8
      0.00000     4.70000     2.70000  # vert   9
      2.10000     4.70000     2.70000  # vert  10
      2.10000     6.33200     2.70000  # vert  11
      2.10000     3.10000     2.70000  # vert  12
      3.30000     3.70000     2.70000  # vert  13
      2.10000     3.70000     2.70000  # vert  14
      3.30000     6.33200     4.92500  # vert  15
      0.00000     6.33200     4.92500  # vert  16
      2.10000     6.33200     3.40000  # vert  17
      1.10000     6.33200     3.40000  # vert  18
      1.10000     6.33200     4.90000  # vert  19
      2.10000     6.33200     4.90000  # vert  20
      0.00000     4.60000     3.40000  # vert  21
      0.00000     3.50000     3.40000  # vert  22
      0.00000     3.50000     4.90000  # vert  23
      0.00000     4.60000     4.90000  # vert  24
# no of vertices followed by list of associated vert
   5,  1, 12,  2,  6,  5,
   6,  2, 13,  3, 15,  7,  6,
  11,  3, 11,  4, 16, 15,  3, 17, 20, 19, 18, 17,
  12,  1,  5,  8, 16,  4,  9,  1, 21, 24, 23, 22, 21,
   4,  5,  6,  7,  8,
   5, 13, 14, 10, 11,  3,
   4, 10,  9,  4, 11,
   5, 12,  1,  9, 10, 14,
   4,  2, 12, 14, 13,
   4, 15, 16,  8,  7,
   4, 17, 18, 19, 20,
   4, 21, 22, 23, 24,
# unused index
  0 0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, hallup_r      OPAQ  VERT  partition    hall_up        
  2, bed_2_r       OPAQ  VERT  partition    bed_2          
  3, bed3_ex_n     OPAQ  VERT  air_gap_int  bed_3_air_gap  
  4, bed3_ex_w     OPAQ  VERT  ext_wall_up  EXTERIOR       
  5, loft_r        OPAQ  CEIL  ceil_loft    loft           
  6, kitch_r       OPAQ  FLOR  ceiling_rev  kitchen        
  7, bath_r        OPAQ  FLOR  ceiling_rev  bathroom       
  8, hall_r        OPAQ  FLOR  ceiling_rev  hall           
  9, liv_r         OPAQ  FLOR  ceiling_rev  living_room    
 10, loft_n_r      OPAQ  VERT  t_bridge     t_bridge       
 11, n_glaz        TRAN  VERT  glazing      EXTERIOR       
 12, w_glaz        TRAN  VERT  glazing      EXTERIOR       
# base
  6  7  8  9  0  0    10.67
