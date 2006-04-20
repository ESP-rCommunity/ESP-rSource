# geometry of living_room defined in: ../zones/living_room.geo
GEN  living_room  living_room describes a...  # type, name, descr
      16       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.58800     8.29400     0.00000  # vert   1
      6.41100     7.17500     0.00000  # vert   2
      5.27500     6.87100     0.00000  # vert   3
      6.09100     3.82800     0.00000  # vert   4
     11.40300     5.25100     0.00000  # vert   5
     10.58800     8.29400     2.40000  # vert   6
      7.25400     7.40100     2.40000  # vert   7
      6.41100     7.17500     2.40000  # vert   8
      5.27500     6.87100     2.40000  # vert   9
      6.09100     3.82800     2.40000  # vert  10
      8.06900     4.35800     2.40000  # vert  11
     11.40300     5.25100     2.40000  # vert  12
      9.80500     4.82300     0.00000  # vert  13
      9.80500     4.82300     2.10000  # vert  14
      6.32700     3.89100     2.10000  # vert  15
      6.32700     3.89100     0.00000  # vert  16
# no of vertices followed by list of associated vert
   5,  1,  2,  8,  7,  6,
   4,  2,  3,  9,  8,
   4,  3,  4, 10,  9,
   9,  4, 16, 15, 14, 13,  5, 12, 11, 10,
   4,  5,  1,  6, 12,
   4,  6,  7, 11, 12,
   5,  7,  8,  9, 10, 11,
   7,  5, 13, 16,  4,  3,  2,  1,
   4, 13, 14, 15, 16,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, hall_liv      OPAQ  VERT  int_part_1   hall           
  2, kit_liv       OPAQ  VERT  int_part_1   kitchen        
  3, din_liv       OPAQ  VERT  int_part_1   dining_room    
  4, liv_con       OPAQ  VERT  ext_wall     conservatory   
  5, e_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  6, ceil_b2       OPAQ  CEIL  grnd_ceiling bedroom_2      
  7, ceil_b4       OPAQ  CEIL  grnd_ceiling bedroom_4      
  8, floor         OPAQ  FLOR  grnd_floor   GROUND         
  9, glazing       TRAN  VERT  double_glaz  conservatory   
# base
  8  0  0  0  0  0    17.33 0
