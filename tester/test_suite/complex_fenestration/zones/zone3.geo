# geometry of zone3 defined in: ../zones/zone3.geo
GEN  zone3  zone3 describes a  # type, name, descr
      20       9   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     10.00000     0.00000     0.00000  # vert   1
     15.00000     0.00000     0.00000  # vert   2
     15.00000     3.00000     0.00000  # vert   3
     10.00000     3.00000     0.00000  # vert   4
     10.00000     0.00000     2.70000  # vert   5
     15.00000     0.00000     2.70000  # vert   6
     15.00000     3.00000     2.70000  # vert   7
     10.00000     3.00000     2.70000  # vert   8
     10.25000     0.00000     0.25000  # vert   9
     12.25000     0.00000     0.25000  # vert  10
     12.25000     0.00000     2.25000  # vert  11
     10.25000     0.00000     2.25000  # vert  12
     12.75000     0.00000     0.25000  # vert  13
     14.75000     0.00000     0.25000  # vert  14
     14.75000     0.00000     2.25000  # vert  15
     12.75000     0.00000     2.25000  # vert  16
     15.00000     0.50000     0.25000  # vert  17
     15.00000     2.50000     0.25000  # vert  18
     15.00000     2.50000     2.25000  # vert  19
     15.00000     0.50000     2.25000  # vert  20
# no of vertices followed by list of associated vert
  16,  1,  2, 14, 13, 16, 15, 14,  2,  6,  5,  1,  9, 12, 11, 10,  9,
  10,  2,  3,  7,  6,  2, 17, 20, 19, 18, 17,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
   4, 17, 18, 19, 20,
# unused index
 0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Wall-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Wall-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Wall-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Wall-4        OPAQ  VERT  partition    ANOTHER        
  5, Top-5         OPAQ  CEIL  extern_wall  EXTERIOR       
  6, Base-6        OPAQ  FLOR  extern_wall  EXTERIOR       
  7, window6       CFC   VERT  clr_clr_shd  EXTERIOR       
  8, window7       CFC   VERT  trip_out_shd EXTERIOR       
  9, window8       CFC   VERT  dbl_cfc      EXTERIOR       
# base
  6  0  0  0  0  0    15.00 0
