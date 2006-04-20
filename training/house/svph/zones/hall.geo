# geometry of hall defined in: ../zones/hall.geo
GEN  hall  hall describes a...  # type, name, descr
      21      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      9.77300    11.33700     0.00000  # vert   1
      8.85500    11.09100     0.00000  # vert   2
      9.17900     9.88400     0.00000  # vert   3
      7.77800     9.50800     0.00000  # vert   4
      7.45500    10.71600     0.00000  # vert   5
      5.59600    10.21800     0.00000  # vert   6
      6.41200     7.17500     0.00000  # vert   7
     10.58800     8.29400     0.00000  # vert   8
      9.77300    11.33700     2.40000  # vert   9
      8.85500    11.09100     2.40000  # vert  10
      9.17900     9.88400     2.40000  # vert  11
      7.77800     9.50800     2.40000  # vert  12
      7.45500    10.71600     2.40000  # vert  13
      5.59600    10.21800     2.40000  # vert  14
      6.41200     7.17500     2.40000  # vert  15
      8.27000     7.67300     2.40000  # vert  16
     10.58800     8.29400     2.40000  # vert  17
      9.57981    11.28523     1.48000  # vert  18
      8.97129    11.12216     1.48000  # vert  19
      8.97129    11.12216     2.11000  # vert  20
      9.57981    11.28523     2.11000  # vert  21
# no of vertices followed by list of associated vert
  10,  1,  2, 10,  9,  1, 18, 21, 20, 19, 18,
   4,  2,  3, 11, 10,
   4,  3,  4, 12, 11,
   4,  4,  5, 13, 12,
   4,  5,  6, 14, 13,
   4,  6,  7, 15, 14,
   5,  7,  8, 17, 16, 15,
   4,  8,  1,  9, 17,
   6,  9, 10, 11, 12, 16, 17,
   5, 13, 14, 15, 16, 12,
   8,  8,  7,  6,  5,  4,  3,  2,  1,
   4, 18, 19, 20, 21,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, north_b       OPAQ  VERT  ext_wall     EXTERIOR       
  2, p_lobbya      OPAQ  VERT  int_part_1   int_lobby      
  3, p_lobbyb      OPAQ  VERT  int_part_1   int_lobby      
  4, p_lobbyc      OPAQ  VERT  int_part_1   int_lobby      
  5, north_c       OPAQ  VERT  ext_wall     EXTERIOR       
  6, p_kitchen     OPAQ  VERT  int_part_1   kitchen        
  7, p_living      OPAQ  VERT  int_part_1   living_room    
  8, east          OPAQ  VERT  ext_wall     EXTERIOR       
  9, ceil_b3       OPAQ  CEIL  grnd_ceiling bedroom_3      
 10, ceil_z10      OPAQ  CEIL  grnd_ceiling Zone-10        
 11, floor         OPAQ  FLOR  grnd_floor   GROUND         
 12, glass         TRAN  VERT  double_glaz  EXTERIOR       
# base
 11  0  0  0  0  0    11.81 0
