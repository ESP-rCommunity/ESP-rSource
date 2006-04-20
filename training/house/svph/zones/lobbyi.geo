# geometry of int_lobby defined in: ../zones/lobbyi.geo
GEN  int_lobby  int_lobby describes a...  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.85500    11.09100     0.00000  # vert   1
      7.45400    10.71600     0.00000  # vert   2
      7.77800     9.50800     0.00000  # vert   3
      9.17900     9.88400     0.00000  # vert   4
      8.85500    11.09100     2.40000  # vert   5
      7.45400    10.71600     2.40000  # vert   6
      7.77800     9.50800     2.40000  # vert   7
      9.17900     9.88400     2.40000  # vert   8
      8.54100    11.00700     0.00000  # vert   9
      7.67200    10.77400     0.00000  # vert  10
      7.67200    10.77400     2.10000  # vert  11
      8.54100    11.00700     2.10000  # vert  12
# no of vertices followed by list of associated vert
   8,  1,  9, 12, 11, 10,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   6,  1,  4,  3,  2, 10,  9,
   4,  9, 10, 11, 12,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, n_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  2, inl_hal       OPAQ  VERT  int_part_1   hall           
  3, inl_halb      OPAQ  VERT  int_part_1   hall           
  4, inl_halc      OPAQ  VERT  int_part_1   hall           
  5, ceil          OPAQ  CEIL  grnd_ceiling bedroom_3      
  6, floor         OPAQ  FLOR  grnd_floor   GROUND         
  7, door          OPAQ  VERT  door         EXTERIOR       
# base
  6  0  0  0  0  0     1.81 0
