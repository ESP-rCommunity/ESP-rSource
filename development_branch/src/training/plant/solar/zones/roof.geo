# geometry of roof defined in: ../zones/roof.geo
GEN  roof  roof describes a...  # type, name, descr
      27      14   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      1.00000     1.00000     2.70000  # vert   1
      5.50000     1.00000     2.70000  # vert   2
      5.50000     4.50000     2.70000  # vert   3
      1.00000     4.50000     2.70000  # vert   4
     10.00000     1.00000     2.70000  # vert   5
     14.50000     1.00000     2.70000  # vert   6
     19.00000     1.00000     2.70000  # vert   7
     19.00000     4.50000     2.70000  # vert   8
     19.00000     6.00000     2.70000  # vert   9
      1.00000     6.00000     2.70000  # vert  10
     14.50000     4.50000     2.70000  # vert  11
     10.00000     4.50000     2.70000  # vert  12
      1.00000     3.50000     5.00000  # vert  13
     19.00000     3.50000     5.00000  # vert  14
     10.00000     3.50000     5.00000  # vert  15
      2.00000     1.73600     3.37700  # vert  16
      8.60000     1.73600     3.37700  # vert  17
      8.60000     2.84000     4.39300  # vert  18
      2.00000     2.84000     4.39300  # vert  19
     11.00000     1.73600     3.37700  # vert  20
     17.60000     1.73600     3.37700  # vert  21
     17.60000     2.84000     4.39300  # vert  22
     11.00000     2.84000     4.39300  # vert  23
     13.20000     1.73600     3.37700  # vert  24
     15.40000     1.73600     3.37700  # vert  25
     15.40000     2.84000     4.39300  # vert  26
     13.20000     2.84000     4.39300  # vert  27
# no of vertices followed by list of associated vert
   4,  2,  1,  4,  3,
   4,  5,  2,  3, 12,
   4,  6,  5, 12, 11,
   4,  7,  6, 11,  8,
   7,  8, 11, 12,  3,  4, 10,  9,
  11,  1,  2,  5, 15, 13,  1, 16, 19, 18, 17, 16,
   5,  9, 10, 13, 15, 14,
   4, 10,  4,  1, 13,
   4,  7,  8,  9, 14,
  15,  5,  6,  7, 14, 15,  5, 20, 23, 27, 26, 22, 21, 25, 24, 20,
   4, 16, 17, 18, 19,
   4, 20, 24, 27, 23,
   4, 24, 25, 26, 27,
   4, 25, 21, 22, 26,
# unused index
 0,0,0,0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, ceil_dg       OPAQ  FLOR  ceiling_rev  direct_gain    
  2, ceil_rad      OPAQ  FLOR  ceiling_rev  rad_test       
  3, ceil_exch     OPAQ  FLOR  ceiling_rev  heatexch       
  4, ceil_mix      OPAQ  FLOR  ceiling_rev  mixed_test     
  5, ceil_pasg     OPAQ  FLOR  ceiling_rev  passage        
  6, south_rfl     OPAQ  SLOP  roof         EXTERIOR       
  7, north_rf      OPAQ  SLOP  roof         EXTERIOR       
  8, left_rf       OPAQ  VERT  ext_wall     EXTERIOR       
  9, right_rf      OPAQ  VERT  ext_wall     EXTERIOR       
 10, south_rfr     OPAQ  SLOP  roof         EXTERIOR       
 11, liqd_col      OPAQ  SLOP  colct_frame  EXTERIOR       
 12, air_l         OPAQ  SLOP  colct_frame  air_col_left   
 13, air_m         OPAQ  SLOP  colct_frame  air_col_mid    
 14, air_r         OPAQ  SLOP  colct_frame  air_col_right  
# base
  1  2  3  4  5  0    90.00 0
