# geometry of bedroom_3 defined in: ../zones/bedroom_3.geo
GEN  bedroom_3  bedroom_3 describes a...  # type, name, descr
      15       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      9.77300    11.33700     2.40000  # vert   1
      8.85500    11.09100     2.40000  # vert   2
      7.45500    10.71600     2.40000  # vert   3
      7.77800     9.50800     2.40000  # vert   4
      8.27000     7.67300     2.40000  # vert   5
     10.58800     8.29400     2.40000  # vert   6
      9.17900     9.88400     2.40000  # vert   7
      9.77300    11.33700     4.80000  # vert   8
      7.45500    10.71600     4.80000  # vert   9
      8.27000     7.67300     4.80000  # vert  10
     10.58800     8.29400     4.80000  # vert  11
      8.73500    11.05700     3.60000  # vert  12
      7.57600    10.74700     3.60000  # vert  13
      7.57600    10.74800     4.50000  # vert  14
      8.73500    11.05800     4.50000  # vert  15
# no of vertices followed by list of associated vert
  11,  1,  2,  3,  9,  8,  1, 12, 15, 14, 13, 12,
   5,  3,  4,  5, 10,  9,
   4,  5,  6, 11, 10,
   4,  6,  1,  8, 11,
   4,  8,  9, 10, 11,
   6,  6,  5,  4,  7,  2,  1,
   4,  7,  4,  3,  2,
   4, 12, 13, 14, 15,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, n_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  2, p_hall        OPAQ  VERT  int_part_1   Upper_hall     
  3, p_bed2        OPAQ  VERT  int_part_1   bedroom_2      
  4, e_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  5, ceil          OPAQ  CEIL  1st_ceiling  loft           
  6, flr_hall      OPAQ  FLOR  grd_ceil_rev hall           
  7, flr_lobby     OPAQ  FLOR  grd_ceil_rev int_lobby      
  8, window        TRAN  VERT  double_glaz  EXTERIOR       
# base
  0  0  0  0  0  0     7.56 1
