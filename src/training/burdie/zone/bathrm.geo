# geometry of bathroom defined in: ../zone/bathrm.geo
GEN  bathroom  bathroom describes the lower level toilet  # type, name, descr
      14       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     4.70000     0.00000  # vert   1
      2.10000     4.70000     0.00000  # vert   2
      2.10000     6.40000     0.00000  # vert   3
      0.00000     6.40000     0.00000  # vert   4
      0.00000     4.70000     2.70000  # vert   5
      2.10000     4.70000     2.70000  # vert   6
      2.10000     6.40000     2.70000  # vert   7
      0.00000     6.40000     2.70000  # vert   8
      2.00000     6.40000     0.70000  # vert   9
      0.90000     6.40000     0.70000  # vert  10
      0.90000     6.40000     2.30000  # vert  11
      2.00000     6.40000     2.30000  # vert  12
      0.00000     6.33600     2.70000  # vert  13
      2.10000     6.33600     2.70000  # vert  14
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   5,  2,  3,  7, 14,  6,
  10,  3,  4,  8,  7,  3,  9, 12, 11, 10,  9,
   5,  4,  1,  5, 13,  8,
   4,  5,  6, 14, 13,
   4,  4,  3,  2,  1,
   4,  9, 10, 11, 12,
   4, 13, 14,  7,  8,
# unused index
  0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, hall_r        OPAQ  VERT  partition    hall           
  2, kitch_r       OPAQ  VERT  partition    kitchen        
  3, bath_ex_n     OPAQ  VERT  ext_wall_low EXTERIOR       
  4, bath_ex_w     OPAQ  VERT  ext_wall_low EXTERIOR       
  5, bed_1_r       OPAQ  CEIL  ceiling      bed_3          
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, n_glaz        TRAN  VERT  glazing      EXTERIOR       
  8, air_gap_r     OPAQ  CEIL  ceiling      bed_3_air_gap  
# base
  6  0  0  0  0  0     3.57
