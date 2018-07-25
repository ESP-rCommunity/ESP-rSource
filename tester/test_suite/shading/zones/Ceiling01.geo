# geometry of Ceiling01 defined in: ../zones/Ceiling01.geo
GEN  Ceiling01  describes the roof or attic zone  # type, name, descr
       6       5   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     -1.00000     0.75000     3.02000  # vert   1
    -10.74600     0.75000     3.02000  # vert   2
    -10.74600    10.49600     3.02000  # vert   3
     -1.00000    10.49600     3.02000  # vert   4
     -1.00000     5.62300     4.64417  # vert   5
    -10.74600     5.62300     4.64417  # vert   6
# no of vertices followed by list of associated vert
   4,  1,  2,  3,  4,
   4,  1,  5,  6,  2,
   3,  2,  6,  3,
   4,  3,  6,  5,  4,
   3,  4,  5,  1,
# unused index
 0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, to_main       OPAQ  FLOR  ceiling_rev  ANOTHER        
  2, WestSlope     OPAQ  SLOP  asphalt      EXTERIOR       
  3, NorthGable    OPAQ  VERT  cladding     EXTERIOR       
  4, EastSlope     OPAQ  SLOP  asphalt      EXTERIOR       
  5, SouthGable    OPAQ  VERT  cladding     EXTERIOR       
# base
  1  0  0  0  0  0    94.98 0
