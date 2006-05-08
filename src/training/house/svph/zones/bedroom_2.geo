# geometry of bedroom_2 defined in: ../zones/bedroom_2.geo
GEN  bedroom_2  bedroom_2 describes a...  # type, name, descr
      16       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.58800     8.29400     2.40000  # vert   1
      8.27000     7.67300     2.40000  # vert   2
      7.25400     7.40100     2.40000  # vert   3
      8.06900     4.35800     2.40000  # vert   4
     11.40300     5.25100     2.40000  # vert   5
     10.58800     8.29400     4.80000  # vert   6
      8.27000     7.67300     4.80000  # vert   7
      7.25400     7.40100     4.80000  # vert   8
      8.06900     4.35800     4.80000  # vert   9
     11.40300     5.25100     4.80000  # vert  10
      8.06900     4.35800     2.70000  # vert  11
     11.40300     5.25100     2.70000  # vert  12
      9.04700     4.62000     2.80000  # vert  13
     10.88200     5.11100     2.80000  # vert  14
     10.88200     5.11100     4.60000  # vert  15
      9.04700     4.62000     4.60000  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2,  7,  6,
   4,  2,  3,  8,  7,
   5,  3,  4, 11,  9,  8,
  10, 11, 12, 10,  9, 11, 13, 16, 15, 14, 13,
   4,  4,  5, 12, 11,
   5,  1,  6, 10, 12,  5,
   5,  9, 10,  6,  7,  8,
   5,  3,  2,  1,  5,  4,
   4, 13, 14, 15, 16,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, p_bed3        OPAQ  VERT  int_part_1   bedroom_3      
  2, p_hall        OPAQ  VERT  int_part_1   Upper_hall     
  3, p_bed4        OPAQ  VERT  int_part_2   bedroom_4      
  4, s_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  5, p_conserv     OPAQ  VERT  ext_wall     conservatory   
  6, e_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  7, ceil          OPAQ  CEIL  1st_ceiling  loft           
  8, flr_liv       OPAQ  FLOR  grd_ceil_rev living_room    
  9, window        TRAN  VERT  double_glaz  EXTERIOR       
# base
  8  0  0  0  0  0    10.87 0
