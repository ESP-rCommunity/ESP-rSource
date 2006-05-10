# geometry of air_col_mid defined in: ../zones/air_col_mid.geo
GEN  air_col_mid  air_col_mid describes a...  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     13.20000     1.73600     3.37700  # vert   1
     15.40000     1.73600     3.37700  # vert   2
     15.40000     2.84000     4.39300  # vert   3
     13.20000     2.84000     4.39300  # vert   4
     13.20000     1.63600     3.47700  # vert   5
     15.40000     1.63600     3.47700  # vert   6
     15.40000     2.74000     4.49300  # vert   7
     13.20000     2.74000     4.49300  # vert   8
# no of vertices followed by list of associated vert
   4,  2,  1,  4,  3,
   4,  5,  6,  7,  8,
   4,  4,  1,  5,  8,
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, base          OPAQ  SLOP  colct_frame  roof           
  2, cover         TRAN  SLOP  dbl_glz      EXTERIOR       
  3, left          OPAQ  VERT  colct_frame  air_col_left   
  4, bot_frame     OPAQ  SLOP  colct_frame  EXTERIOR       
  5, right         OPAQ  VERT  colct_frame  air_col_right  
  6, top_frame     OPAQ  SLOP  colct_frame  EXTERIOR       
# base
  0  0  0  0  0  0     0.00 0
