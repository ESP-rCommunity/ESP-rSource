# geometry of bedroom_1 defined in: ../zones/bedroom_1.geo
GEN  bedroom_1  bedroom_1 describes a...  # type, name, descr
      21      12   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      5.01900     7.82800     2.40000  # vert   1
      1.92800     7.00000     2.40000  # vert   2
      2.18500     6.04300     2.40000  # vert   3
      2.62500     4.40000     2.40000  # vert   4
      3.00000     3.00000     2.40000  # vert   5
      5.60800     3.69900     2.40000  # vert   6
      5.34900     4.66500     2.40000  # vert   7
      5.83200     4.79400     2.40000  # vert   8
      5.27500     6.87100     2.40000  # vert   9
      1.92800     7.00000     4.26200  # vert  10
      5.01900     7.82800     4.80000  # vert  11
      1.92800     7.00000     4.80000  # vert  12
      3.00000     3.00000     4.80000  # vert  13
      5.60800     3.69900     4.80000  # vert  14
      5.34900     4.66500     4.80000  # vert  15
      5.83200     4.79400     4.80000  # vert  16
      5.27500     6.87100     4.80000  # vert  17
      3.52200     3.14000     2.70000  # vert  18
      5.35700     3.63200     2.70000  # vert  19
      5.35700     3.63200     4.50000  # vert  20
      3.52200     3.14000     4.50000  # vert  21
# no of vertices followed by list of associated vert
   5,  1,  2, 10, 12, 11,
   4,  2,  3,  4, 10,
   5, 10,  4,  5, 13, 12,
  10,  5,  6, 14, 13,  5, 18, 21, 20, 19, 18,
   4,  6,  7, 15, 14,
   4,  7,  8, 16, 15,
   4,  8,  9, 17, 16,
   4,  9,  1, 11, 17,
   7, 11, 12, 13, 14, 15, 16, 17,
   4,  9,  3,  2,  1,
   7,  7,  6,  5,  4,  3,  9,  8,
   4, 18, 19, 20, 21,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, p_bath_b1     OPAQ  VERT  int_part_2   bathroom       
  2, p_garage      OPAQ  VERT  ext_wall     garage         
  3, w_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  4, s_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  5, p_bed4b       OPAQ  VERT  int_part_1   bedroom_4      
  6, p_bed4a       OPAQ  VERT  int_part_1   bedroom_4      
  7, p_bed4        OPAQ  VERT  int_part_1   bedroom_4      
  8, p_hall        OPAQ  VERT  int_part_1   Upper_hall     
  9, ceil          OPAQ  CEIL  1st_ceiling  loft           
 10, flr_kit       OPAQ  FLOR  grd_ceil_rev kitchen        
 11, flr_din       OPAQ  FLOR  grd_ceil_rev dining_room    
 12, window        TRAN  VERT  double_glaz  EXTERIOR       
# base
  0  0  0  0  0  0    12.75 1
