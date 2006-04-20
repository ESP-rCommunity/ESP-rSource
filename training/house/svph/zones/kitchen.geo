# geometry of kitchen defined in: ../zones/kitchen.geo
GEN  kitchen  kitchen describes a...  # type, name, descr
      20      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      5.59700    10.21800     0.00000  # vert   1
      2.45200     9.37500     0.00000  # vert   2
      1.37000     9.08500     0.00000  # vert   3
      2.18500     6.04300     0.00000  # vert   4
      5.27600     6.87100     0.00000  # vert   5
      6.41200     7.17500     0.00000  # vert   6
      5.59700    10.21800     2.40000  # vert   7
      4.75800     9.99300     2.40000  # vert   8
      2.45200     9.37500     2.40000  # vert   9
      1.37000     9.08500     2.40000  # vert  10
      1.92900     7.00000     2.40000  # vert  11
      2.18500     6.04300     2.40000  # vert  12
      5.27600     6.87100     2.40000  # vert  13
      6.41200     7.17500     2.40000  # vert  14
      5.31600     7.90800     2.40000  # vert  15
      5.02000     7.82800     2.40000  # vert  16
      5.04500    10.07300     1.05000  # vert  17
      3.30600     9.60700     1.05000  # vert  18
      3.30700     9.60500     2.10000  # vert  19
      5.04500    10.07100     2.10000  # vert  20
# no of vertices followed by list of associated vert
  11,  1,  2,  9,  8,  7,  1, 17, 20, 19, 18, 17,
   4,  2,  3, 10,  9,
   5,  3,  4, 12, 11, 10,
   4,  4,  5, 13, 12,
   4,  5,  6, 14, 13,
   4,  6,  1,  7, 14,
   6,  7,  8, 15, 16, 13, 14,
   6,  8,  9, 10, 11, 16, 15,
   4, 16, 11, 12, 13,
   6,  6,  5,  4,  3,  2,  1,
   4, 17, 18, 19, 20,
# unused index
 0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, n_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  2, exl_kit       OPAQ  VERT  int_part_1   ext_lobby      
  3, gar_kit       OPAQ  VERT  ext_wall     garage         
  4, kit_din       OPAQ  VERT  int_part_1   dining_room    
  5, kit_liv       OPAQ  VERT  int_part_1   living_room    
  6, kit_hall      OPAQ  VERT  int_part_1   hall           
  7, kit_uh        OPAQ  CEIL  grnd_ceiling Upper_hall     
  8, ceil_k_bth    OPAQ  CEIL  grnd_ceiling bathroom       
  9, ceil_k_b1     OPAQ  CEIL  grnd_ceiling bedroom_1      
 10, floor         OPAQ  FLOR  grnd_floor   GROUND         
 11, window        TRAN  VERT  double_glaz  EXTERIOR       
# base
 10  0  0  0  0  0    13.78 0
