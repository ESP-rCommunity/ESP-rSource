# geometry of hall defined in: ../zone/hall.geo
GEN  hall  hall describes the lower portion of the entry  # type, name, descr
      20      10   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     0.00000  # vert   1
      2.10000     0.00000     0.00000  # vert   2
      2.10000     4.70000     0.00000  # vert   3
      0.00000     4.70000     0.00000  # vert   4
      0.00000     0.00000     2.70000  # vert   5
      2.10000     0.00000     2.70000  # vert   6
      2.10000     4.70000     2.70000  # vert   7
      0.00000     4.70000     2.70000  # vert   8
      0.00000     3.10000     2.70000  # vert   9
      2.10000     3.10000     2.70000  # vert  10
      2.10000     3.70000     0.00000  # vert  11
      2.10000     3.70000     2.70000  # vert  12
      1.00000     0.00000     0.00000  # vert  13
      2.00000     0.00000     0.00000  # vert  14
      2.00000     0.00000     2.00000  # vert  15
      1.00000     0.00000     2.00000  # vert  16
      0.00000     4.40000     0.90000  # vert  17
      0.00000     3.30000     0.90000  # vert  18
      0.00000     3.30000     2.20000  # vert  19
      0.00000     4.40000     2.20000  # vert  20
# no of vertices followed by list of associated vert
   8,  1, 13, 16, 15, 14,  2,  6,  5,
   5,  2, 11, 12, 10,  6,
   4,  3,  4,  8,  7,
  11,  4,  1,  5,  9,  8,  4, 17, 20, 19, 18, 17,
   5,  9, 10, 12,  7,  8,
   7,  4,  3, 11,  2, 14, 13,  1,
   4, 10,  9,  5,  6,
   4, 11,  3,  7, 12,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
# unused index
  0 0 0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, hall_ex_s     OPAQ  VERT  ext_wall_low EXTERIOR       
  2, livrm_r       OPAQ  VERT  partition    living_room    
  3, bathrm_r      OPAQ  VERT  partition    bathroom       
  4, hall_ex_w     OPAQ  VERT  ext_wall_low EXTERIOR       
  5, bed_3_r       OPAQ  CEIL  ceiling      bed_3          
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, hall_up_r     OPAQ  CEIL  ceiling      hall_up        
  8, kitchrev      OPAQ  VERT  partition    kitchen        
  9, front_door    OPAQ  VERT  door         EXTERIOR       
 10, w_glaz        TRAN  VERT  glazing      EXTERIOR       
# base
  6  0  0  0  0  0     9.87
