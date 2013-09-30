# geometry of attic defined in: user/L_NW_Door2WindowsL/zones/attic.geo
GEN  attic  describes the roof or attic zone  # type, name, descr
       9        7 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    10.00000  4.00000  4.94000   # vert 1  
    4.00000  4.00000  4.94000   # vert 2  
    4.00000  0.00000  4.94000   # vert 3  
    0.00000  0.00000  4.94000   # vert 4  
    0.00000  8.60000  4.94000   # vert 5  
    10.00000  8.60000  4.94000   # vert 6  
    2.00000  0.00000  5.60600   # vert 7  
    2.00000  6.30000  5.60600   # vert 8  
    10.00000  6.30000  5.60600   # vert 9  
# no of vertices followed by list of associated vert
   6,   1,   2,   3,   4,   5,   6,
   4,   1,   9,   8,   2,
   4,   2,   8,   7,   3,
   3,   3,   7,   4,
   4,   4,   7,   8,   5,
   4,   5,   8,   9,   6,
   3,   6,   9,   1,
# unused index
 0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, to_main       OPAQ  FLOR  ceiling_r    main           
  2, SecondWestSlope  OPAQ  SLOP  asphalt      EXTERIOR       
  3, FirstSouthSlope  OPAQ  SLOP  asphalt      EXTERIOR       
  4, WestGableEnd  OPAQ  VERT  cladding     EXTERIOR       
  5, NorthSlope    OPAQ  SLOP  asphalt      EXTERIOR       
  6, EastSlope     OPAQ  SLOP  asphalt      EXTERIOR       
  7, SouthGableEnd  OPAQ  VERT  cladding     EXTERIOR       
# base
1   0   0   0   0   0   62.00    
