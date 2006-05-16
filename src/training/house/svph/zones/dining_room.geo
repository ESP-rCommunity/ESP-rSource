# geometry of dining_room defined in: ../zones/dining_room.geo
GEN  dining_room  dining_room describes a...  # type, name, descr
      17       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      5.27600     6.87100     0.00000  # vert   1
      2.18500     6.04300     0.00000  # vert   2
      2.62500     4.40100     0.00000  # vert   3
      3.00000     3.00000     0.00000  # vert   4
      6.09100     3.82800     0.00000  # vert   5
      5.27600     6.87100     2.40000  # vert   6
      2.18500     6.04300     2.40000  # vert   7
      2.62500     4.40100     2.40000  # vert   8
      3.00000     3.00000     2.40000  # vert   9
      5.60800     3.69900     2.40000  # vert  10
      6.09100     3.82800     2.40000  # vert  11
      5.83200     4.79400     2.40000  # vert  12
      5.34900     4.66500     2.40000  # vert  13
      3.57200     3.14400     0.06000  # vert  14
      5.31100     3.61000     0.06000  # vert  15
      5.30900     3.61700     2.16000  # vert  16
      3.57000     3.15200     2.16000  # vert  17
# no of vertices followed by list of associated vert
   4,  1,  2,  7,  6,
   4,  2,  3,  8,  7,
   4,  3,  4,  9,  8,
  11,  4,  5, 11, 10,  9,  4, 14, 17, 16, 15, 14,
   5,  5,  1,  6, 12, 11,
   4, 12, 13, 10, 11,
   7,  6,  7,  8,  9, 10, 13, 12,
   5,  5,  4,  3,  2,  1,
   4, 14, 15, 16, 17,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, kit_din       OPAQ  VERT  int_part_1   kitchen        
  2, gar_kit       OPAQ  VERT  ext_wall     garage         
  3, w_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  4, s_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  5, din_liv       OPAQ  VERT  int_part_1   living_room    
  6, ceil_b4       OPAQ  CEIL  grnd_ceiling bedroom_4      
  7, ceil_b1       OPAQ  CEIL  grnd_ceiling bedroom_1      
  8, floor         OPAQ  FLOR  grnd_floor   GROUND         
  9, glazing       TRAN  VERT  double_glaz  EXTERIOR       
# base
  8  0  0  0  0  0    10.08 0
