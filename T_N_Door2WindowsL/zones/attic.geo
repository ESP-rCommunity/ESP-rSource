# geometry of attic defined in: user/T_N_Door2WindowsL/zones/attic.geo
GEN  attic  describes the roof or attic zone  # type, name, descr
      12        9 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    2.00000  0.00000  4.88000   # vert 1  
    2.00000  4.00000  4.88000   # vert 2  
    0.00000  4.00000  4.88000   # vert 3  
    0.00000  9.00000  4.88000   # vert 4  
    10.00000  9.00000  4.88000   # vert 5  
    10.00000  4.00000  4.88000   # vert 6  
    7.00000  4.00000  4.88000   # vert 7  
    7.00000  0.00000  4.88000   # vert 8  
    0.00000  6.50000  5.50500   # vert 9  
    4.50000  6.50000  5.50500   # vert 10 
    10.00000  6.50000  5.50500   # vert 11 
    4.50000  0.00000  5.50500   # vert 12 
# no of vertices followed by list of associated vert
   8,   1,   2,   3,   4,   5,   6,   7,   8,
   4,   1,  12,  10,   2,
   4,   2,  10,   9,   3,
   3,   3,   9,   4,
   4,   4,   9,  11,   5,
   3,   5,  11,   6,
   4,   6,  11,  10,   7,
   4,   7,  10,  12,   8,
   3,   8,  12,   1,
# unused index
 0   0   0   0   0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, to_main       OPAQ  FLOR  ceiling_r    main           
  2, WestSlope     OPAQ  SLOP  asphalt      EXTERIOR       
  3, SouthWestSlope  OPAQ  SLOP  asphalt      EXTERIOR       
  4, WestGable     OPAQ  VERT  cladding     EXTERIOR       
  5, NorthSlope    OPAQ  SLOP  asphalt      EXTERIOR       
  6, EastGable     OPAQ  VERT  cladding     EXTERIOR       
  7, SouthEastSlope  OPAQ  SLOP  asphalt      EXTERIOR       
  8, EastSlope     OPAQ  SLOP  asphalt      EXTERIOR       
  9, SouthGable    OPAQ  VERT  cladding     EXTERIOR       
# base
1   0   0   0   0   0   60.00    
