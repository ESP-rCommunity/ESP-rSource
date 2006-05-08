# geometry of store defined in: ../zone/store.geo
GEN  store  store describes the back entrance and store  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      5.20000     3.70000     0.00000  # vert   1
      6.90000     3.70000     0.00000  # vert   2
      6.90000     6.40000     0.00000  # vert   3
      5.20000     6.40000     0.00000  # vert   4
      5.20000     3.70000     2.70000  # vert   5
      6.90000     3.70000     2.70000  # vert   6
      6.90000     6.40000     2.70000  # vert   7
      5.20000     6.40000     2.70000  # vert   8
      6.50000     6.40000     0.00000  # vert   9
      5.50000     6.40000     0.00000  # vert  10
      5.50000     6.40000     2.00000  # vert  11
      6.50000     6.40000     2.00000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   8,  3,  9, 12, 11, 10,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   6,  4, 10,  9,  3,  2,  1,
   4,  9, 10, 11, 12,
# unused index
  0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, liv_r         OPAQ  VERT  partition    living_room    
  2, store_sim_e   OPAQ  VERT  intern_wall  SIMILAR        
  3, store_n_ex    OPAQ  VERT  ext_wall_low EXTERIOR       
  4, kitch_r       OPAQ  VERT  partition    kitchen        
  5, bed_2_r       OPAQ  CEIL  ceiling      bed_2          
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, back_door     OPAQ  VERT  door         EXTERIOR       
# base
  6  0  0  0  0  0     4.59
