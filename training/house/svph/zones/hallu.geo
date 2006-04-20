# geometry of upper_hall defined in: ../zones/hallu.geo
GEN  upper_hall  upper_hall describes a...  # type, name, descr
      19      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      7.45500    10.71600     2.40000  # vert   1
      6.43900    10.44400     2.40000  # vert   2
      4.75700     9.99300     2.40000  # vert   3
      5.31600     7.90800     2.40000  # vert   4
      5.01900     7.82800     2.40000  # vert   5
      5.27600     6.87100     2.40000  # vert   6
      7.25400     7.40100     2.40000  # vert   7
      8.27000     7.67300     2.40000  # vert   8
      7.45500    10.71600     4.80000  # vert   9
      4.75700     9.99300     4.80000  # vert  10
      5.31600     7.90800     4.80000  # vert  11
      5.01900     7.82800     4.80000  # vert  12
      5.27600     6.87100     4.80000  # vert  13
      7.25400     7.40100     4.80000  # vert  14
      8.27000     7.67300     4.80000  # vert  15
      6.68300    10.51600     2.60000  # vert  16
      5.52400    10.20600     2.60000  # vert  17
      5.52500    10.20200     3.68000  # vert  18
      6.68400    10.51300     3.68000  # vert  19
# no of vertices followed by list of associated vert
  11,  1,  2,  3, 10,  9,  1, 16, 19, 18, 17, 16,
   4,  3,  4, 11, 10,
   4,  4,  5, 12, 11,
   4,  5,  6, 13, 12,
   4,  6,  7, 14, 13,
   4,  7,  8, 15, 14,
   4,  8,  1,  9, 15,
   7,  9, 10, 11, 12, 13, 14, 15,
   4,  8,  7,  2,  1,
   6,  7,  6,  5,  4,  3,  2,
   4, 16, 17, 18, 19,
# unused index
 0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, n_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  2, prt_bth       OPAQ  VERT  int_part_2   bathroom       
  3, prt_btha      OPAQ  VERT  int_part_2   bathroom       
  4, prt_b1        OPAQ  VERT  int_part_1   bedroom_1      
  5, prt_b4        OPAQ  VERT  int_part_1   bedroom_4      
  6, prt_b2        OPAQ  VERT  int_part_1   bedroom_2      
  7, prt_h_b3      OPAQ  VERT  int_part_1   bedroom_3      
  8, ceil          OPAQ  CEIL  1st_ceiling  loft           
  9, flr_hall      OPAQ  FLOR  grd_ceil_rev hall           
 10, flr_kit       OPAQ  FLOR  grd_ceil_rev kitchen        
 11, window        TRAN  VERT  double_glaz  EXTERIOR       
# base
  0  0  0  0  0  0     9.10 1
