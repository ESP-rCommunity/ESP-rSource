# geometry of ext_lobby defined in: ../zones/lobbye.geo
GEN  ext_lobby  ext_lobby describes a...  # type, name, descr
      14       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      2.06400    10.82400     0.00000  # vert   1
      0.98200    10.53400     0.00000  # vert   2
      1.37000     9.08500     0.00000  # vert   3
      2.45200     9.37500     0.00000  # vert   4
      2.06400    10.82400     2.40000  # vert   5
      0.98200    10.53400     2.40000  # vert   6
      1.37000     9.08500     2.40000  # vert   7
      2.45200     9.37500     2.40000  # vert   8
      2.45200     9.37500     3.46600  # vert   9
      1.37000     9.08500     3.46600  # vert  10
      1.00800    10.43700     0.00000  # vert  11
      1.24100     9.56800     0.00000  # vert  12
      1.24100     9.56800     2.10000  # vert  13
      1.00800    10.43700     2.10000  # vert  14
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   9,  2, 11, 14, 13, 12,  3,  7, 10,  6,
   4,  3,  4,  8,  7,
   4,  7,  8,  9, 10,
   5,  4,  1,  5,  9,  8,
   4,  5,  6, 10,  9,
   6,  4,  3, 12, 11,  2,  1,
   4, 11, 12, 13, 14,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, n_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  2, gar_exl       OPAQ  VERT  ext_wall     garage         
  3, exl_kit       OPAQ  VERT  int_part_1   kitchen        
  4, exl_bath      OPAQ  VERT  int_part_1   bathroom       
  5, e_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  6, roof          OPAQ  SLOP  grnd_ceiling EXTERIOR       
  7, floor         OPAQ  FLOR  grnd_floor   GROUND         
  8, door          OPAQ  VERT  door         garage         
# base
  7  0  0  0  0  0     1.68 0
