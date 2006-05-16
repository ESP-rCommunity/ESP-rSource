# geometry of bedroom_4 defined in: ../zones/bedroom_4.geo
GEN  bedroom_4  bedroom_4 describes a...  # type, name, descr
      19      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      7.25400     7.40100     2.40000  # vert   1
      5.27600     6.87100     2.40000  # vert   2
      5.83200     4.79400     2.40000  # vert   3
      5.34900     4.66500     2.40000  # vert   4
      5.60800     3.69900     2.40000  # vert   5
      6.09100     3.82800     2.40000  # vert   6
      8.06900     4.35800     2.40000  # vert   7
      7.25400     7.40100     4.80000  # vert   8
      5.27600     6.87100     4.80000  # vert   9
      5.83200     4.79400     4.80000  # vert  10
      5.34900     4.66500     4.80000  # vert  11
      5.60800     3.69900     4.80000  # vert  12
      8.06900     4.35800     4.80000  # vert  13
      8.06900     4.35800     2.70000  # vert  14
      6.09100     3.82800     2.70000  # vert  15
      5.99400     3.80200     2.80000  # vert  16
      7.83000     4.29300     2.80000  # vert  17
      7.83000     4.29300     4.60000  # vert  18
      5.99400     3.80200     4.60000  # vert  19
# no of vertices followed by list of associated vert
   4,  1,  2,  9,  8,
   4,  2,  3, 10,  9,
   4,  3,  4, 11, 10,
   4,  4,  5, 12, 11,
  12, 15, 14, 13, 12,  5,  6, 15, 16, 19, 18, 17, 16,
   5,  7,  1,  8, 13, 14,
   6,  8,  9, 10, 11, 12, 13,
   5,  7,  6,  3,  2,  1,
   4,  6,  5,  4,  3,
   4,  6,  7, 14, 15,
   4, 16, 17, 18, 19,
# unused index
 0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, p_hall        OPAQ  VERT  int_part_1   Upper_hall     
  2, prt_b1_b4     OPAQ  VERT  int_part_1   bedroom_1      
  3, prt_b1_b4a    OPAQ  VERT  int_part_1   bedroom_1      
  4, prt_b1_b4b    OPAQ  VERT  int_part_1   bedroom_1      
  5, s_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  6, prt_b4_b2     OPAQ  VERT  int_part_2   bedroom_2      
  7, ceil          OPAQ  CEIL  1st_ceiling  loft           
  8, flr_liv       OPAQ  FLOR  grd_ceil_rev living_room    
  9, flr_din       OPAQ  FLOR  grd_ceil_rev dining_room    
 10, Surf-10       OPAQ  VERT  ext_wall     conservatory   
 11, window        TRAN  VERT  double_glaz  EXTERIOR       
# base
  0  0  0  0  0  0     6.95 1
