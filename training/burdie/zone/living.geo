# geometry of living_room defined in: ../zone/living.geo
GEN  living_room  living_room is the ground level lounge  # type, name, descr
      21      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      2.10000     0.00000     0.00000  # vert   1
      6.90000     0.00000     0.00000  # vert   2
      6.90000     3.70000     0.00000  # vert   3
      2.10000     3.70000     0.00000  # vert   4
      2.10000     0.00000     2.70000  # vert   5
      6.90000     0.00000     2.70000  # vert   6
      6.90000     3.70000     2.70000  # vert   7
      2.10000     3.70000     2.70000  # vert   8
      2.10000     3.10000     2.70000  # vert   9
      3.30000     3.10000     2.70000  # vert  10
      3.30000     2.10000     2.70000  # vert  11
      2.30000     2.10000     2.70000  # vert  12
      2.30000     0.00000     2.70000  # vert  13
      3.30000     3.70000     2.70000  # vert  14
      5.20000     3.70000     0.00000  # vert  15
      5.20000     3.70000     2.70000  # vert  16
      6.90000     3.10000     2.70000  # vert  17
      3.40000     0.00000     0.40000  # vert  18
      5.60000     0.00000     0.40000  # vert  19
      5.60000     0.00000     2.30000  # vert  20
      3.40000     0.00000     2.30000  # vert  21
# no of vertices followed by list of associated vert
  11,  1,  2,  6, 13,  5,  1, 18, 21, 20, 19, 18,
   5,  2,  3,  7, 17,  6,
   5, 15,  4,  8, 14, 16,
   5,  4,  1,  5,  9,  8,
   4,  9, 10, 14,  8,
   5,  4, 15,  3,  2,  1,
   6,  9,  5, 13, 12, 11, 10,
   6, 11, 12, 13,  6, 17, 10,
   4,  3, 15, 16,  7,
   5, 16, 14, 10, 17,  7,
   4, 18, 19, 20, 21,
# unused index
  0 0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, liv_ex_s      OPAQ  VERT  ext_wall_low EXTERIOR       
  2, liv_sim_e     OPAQ  VERT  intern_wall  SIMILAR        
  3, kitch_r       OPAQ  VERT  partition    kitchen        
  4, hall_r        OPAQ  VERT  partition    hall           
  5, bed_3_r       OPAQ  CEIL  ceiling      bed_3          
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, hall_up_r     OPAQ  CEIL  ceiling      hall_up        
  8, bed_1_r       OPAQ  CEIL  ceiling      bed_1          
  9, store_r       OPAQ  VERT  partition    store          
 10, bed_2_r       OPAQ  CEIL  ceiling      bed_2          
 11, s_glaz        TRAN  VERT  glazing      EXTERIOR       
# base
  6  0  0  0  0  0    17.76
