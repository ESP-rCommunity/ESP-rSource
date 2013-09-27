# geometry of zone2 defined in: ../zones/zone2.geo
GEN  zone2  zone2 describes a  # type, name, descr
      16       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      5.00000     0.00000     0.00000  # vert   1
     10.00000     0.00000     0.00000  # vert   2
     10.00000     3.00000     0.00000  # vert   3
      5.00000     3.00000     0.00000  # vert   4
      5.00000     0.00000     2.70000  # vert   5
     10.00000     0.00000     2.70000  # vert   6
     10.00000     3.00000     2.70000  # vert   7
      5.00000     3.00000     2.70000  # vert   8
      5.25000     0.00000     0.25000  # vert   9
      7.25000     0.00000     0.25000  # vert  10
      7.25000     0.00000     2.25000  # vert  11
      5.25000     0.00000     2.25000  # vert  12
      7.75000     0.00000     0.25000  # vert  13
      9.75000     0.00000     0.25000  # vert  14
      9.75000     0.00000     2.25000  # vert  15
      7.75000     0.00000     2.25000  # vert  16
# no of vertices followed by list of associated vert
  16,  1,  2, 14, 13, 16, 15, 14,  2,  6,  5,  1,  9, 12, 11, 10,  9,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
   4, 13, 14, 15, 16,
# unused index
 0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Wall-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Wall-2        OPAQ  VERT  partition    ANOTHER        
  3, Wall-3        OPAQ  VERT  extern_wall  EXTERIOR       
  4, Wall-4        OPAQ  VERT  partition    ANOTHER        
  5, Top-5         OPAQ  CEIL  extern_wall  EXTERIOR       
  6, Base-6        OPAQ  FLOR  extern_wall  EXTERIOR       
  7, window4       CFC2  VERT  glz_VBD_glz  EXTERIOR       
  8, window5       CFC2  VERT  glz_VBD2_glz EXTERIOR       
# base
  6  0  0  0  0  0    15.00 0
