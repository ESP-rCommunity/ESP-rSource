# geometry of garage defined in: ../zones/garage.geo
GEN  garage  garage describes a...  # type, name, descr
      23      14   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     -0.95000    10.01700     0.00000  # vert   1
      0.69300     3.88300     0.00000  # vert   2
      2.62500     4.40100     0.00000  # vert   3
      2.18500     6.04300     0.00000  # vert   4
      1.37000     9.08600     0.00000  # vert   5
      0.98200    10.53500     0.00000  # vert   6
     -0.95000    10.01700     2.40000  # vert   7
      0.69300     3.88300     2.40000  # vert   8
      2.62500     4.40100     2.40000  # vert   9
      2.18500     6.04300     2.40000  # vert  10
      1.92900     7.00000     2.40000  # vert  11
      1.37000     9.08600     2.40000  # vert  12
      0.98200    10.53500     2.40000  # vert  13
     -0.56200     8.56800     3.46600  # vert  14
     -0.00300     6.48300     4.26200  # vert  15
      1.92900     7.00000     4.26200  # vert  16
      1.37000     9.08600     3.46600  # vert  17
     -0.13000     6.95500     4.60000  # vert  18
      1.80200     7.47300     4.60000  # vert  19
      1.24100     9.56900     0.00000  # vert  20
      1.00800    10.43800     0.00000  # vert  21
      1.00800    10.43800     2.10000  # vert  22
      1.24100     9.56900     2.10000  # vert  23
# no of vertices followed by list of associated vert
   4,  6,  1,  7, 13,
   7,  1,  2,  8, 15, 18, 14,  7,
   4,  2,  3,  9,  8,
   4,  3,  4, 10,  9,
   5,  4,  5, 12, 11, 10,
   9,  5, 20, 23, 22, 21,  6, 13, 17, 12,
   4,  9, 10, 11, 16,
   5, 11, 12, 17, 19, 16,
   4, 13,  7, 14, 17,
   4, 17, 14, 18, 19,
   4,  8,  9, 16, 15,
   4, 15, 16, 19, 18,
   8,  6, 21, 20,  5,  4,  3,  2,  1,
   4, 20, 21, 22, 23,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, n_ext         OPAQ  VERT  int_part_1   EXTERIOR       
  2, w_ext         OPAQ  VERT  int_part_1   EXTERIOR       
  3, s_ext         OPAQ  VERT  int_part_1   EXTERIOR       
  4, gar_din       OPAQ  VERT  ext_wall_rev dining_room    
  5, gar_kit       OPAQ  VERT  ext_wall_rev kitchen        
  6, gar_exl       OPAQ  VERT  ext_wall_rev ext_lobby      
  7, gar_b1        OPAQ  VERT  ext_wall_rev bedroom_1      
  8, gar_bath      OPAQ  VERT  ext_wall_rev bathroom       
  9, north         OPAQ  SLOP  int_part_1   EXTERIOR       
 10, north_up      OPAQ  SLOP  int_part_1   EXTERIOR       
 11, south         OPAQ  SLOP  int_part_1   EXTERIOR       
 12, south_up      OPAQ  SLOP  int_part_1   EXTERIOR       
 13, floor         OPAQ  FLOR  grnd_floor   GROUND         
 14, door          OPAQ  VERT  door         ext_lobby      
# base
 13  0  0  0  0  0    12.70 0
