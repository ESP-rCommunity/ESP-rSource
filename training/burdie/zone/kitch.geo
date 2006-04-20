# geometry of kitchen defined in: ../zone/kitch.geo
GEN  kitchen  kitchen describes the north facing kitchen  # type, name, descr
      18      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      2.10000     3.70000     0.00000  # vert   1
      5.20000     3.70000     0.00000  # vert   2
      5.20000     6.40000     0.00000  # vert   3
      2.10000     6.40000     0.00000  # vert   4
      2.10000     3.70000     2.70000  # vert   5
      5.20000     3.70000     2.70000  # vert   6
      5.20000     6.40000     2.70000  # vert   7
      2.10000     6.40000     2.70000  # vert   8
      2.10000     4.70000     0.00000  # vert   9
      2.10000     4.70000     2.70000  # vert  10
      3.30000     3.70000     2.70000  # vert  11
      3.30000     6.40000     2.70000  # vert  12
      5.10000     6.40000     0.70000  # vert  13
      2.90000     6.40000     0.70000  # vert  14
      2.90000     6.40000     2.30000  # vert  15
      5.10000     6.40000     2.30000  # vert  16
      2.10000     6.33600     2.70000  # vert  17
      3.30000     6.33600     2.70000  # vert  18
# no of vertices followed by list of associated vert
   5,  1,  2,  6, 11,  5,
   4,  2,  3,  7,  6,
  11,  3,  4,  8, 12,  7,  3, 13, 16, 15, 14, 13,
   4,  9,  1,  5, 10,
   5, 11,  6,  7, 12, 18,
   5,  4,  3,  2,  1,  9,
   5,  4,  9, 10, 17,  8,
   5,  5, 11, 18, 17, 10,
   4, 13, 14, 15, 16,
   4,  8, 17, 18, 12,
# unused index
  0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, liv_r         OPAQ  VERT  partition    living_room    
  2, store_r       OPAQ  VERT  partition    store          
  3, kitch_ex_n    OPAQ  VERT  ext_wall_low EXTERIOR       
  4, hall_r        OPAQ  VERT  partition    hall           
  5, bed_2_r       OPAQ  CEIL  ceiling      bed_2          
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, bathrm_r      OPAQ  VERT  partition    bathroom       
  8, bed_1_r       OPAQ  CEIL  ceiling      bed_3          
  9, n_glaz        TRAN  VERT  glazing      EXTERIOR       
 10, xfirst        OPAQ  CEIL  ceiling      bed_3_air_gap  
# base
  6  0  0  0  0  0     8.37
