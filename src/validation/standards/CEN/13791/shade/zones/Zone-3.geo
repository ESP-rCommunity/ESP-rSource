# geometry of test-3.5 defined in: zones/Zone-3.geo
GEN  test-3.5  test-3.5 describes a...  # type, name, descr
      16       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
     14.00000     0.00000     0.00000  # vert   1
     18.00000     0.00000     0.00000  # vert   2
     18.00000     4.00000     0.00000  # vert   3
     14.00000     4.00000     0.00000  # vert   4
     14.00000     0.00000     3.00000  # vert   5
     18.00000     0.00000     3.00000  # vert   6
     18.00000     4.00000     3.00000  # vert   7
     14.00000     4.00000     3.00000  # vert   8
     15.00000     0.00000     0.50000  # vert   9
     17.00000     0.00000     0.50000  # vert  10
     17.00000     0.00000     2.50000  # vert  11
     15.00000     0.00000     2.50000  # vert  12
     18.00000     1.00000     0.50000  # vert  13
     18.00000     3.00000     0.50000  # vert  14
     18.00000     3.00000     2.50000  # vert  15
     18.00000     1.00000     2.50000  # vert  16
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,
  10,  2,  3,  7,  6,  2, 13, 16, 15, 14, 13,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  4,  3,  2,  1,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    1   6   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Surf-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, Surf-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Surf-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Surf-5        OPAQ  CEIL  extern_wall  EXTERIOR       
  6, Surf-6        OPAQ  FLOR  extern_wall  EXTERIOR       
  7, window_S      OPAQ  VERT  extern_wall  EXTERIOR       
  8, window_E      OPAQ  VERT  extern_wall  EXTERIOR       
# base
  6  0  0  0  0  0    16.00 0
