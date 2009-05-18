# geometry of With_PCM53 defined in: ../zones/With_PCM53.geo
GEN  With_PCM53  With_PCM53 describes a zone with PCM53 material  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      8.00000     0.00000     0.00000  # vert   1
     13.00000     0.00000     0.00000  # vert   2
     13.00000     3.00000     0.00000  # vert   3
      8.00000     3.00000     0.00000  # vert   4
      8.00000     0.00000     2.70000  # vert   5
     13.00000     0.00000     2.70000  # vert   6
     13.00000     3.00000     2.70000  # vert   7
      8.00000     3.00000     2.70000  # vert   8
      9.13069     0.00000     0.61057  # vert   9
     11.86931     0.00000     0.61057  # vert  10
     11.86931     0.00000     2.08943  # vert  11
      9.13069     0.00000     2.08943  # vert  12
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
# unused index
 0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, Wall-1        OPAQ  VERT  extern_wall  EXTERIOR       
  2, Wall-2        OPAQ  VERT  extern_wall  EXTERIOR       
  3, With_PCM_53   OPAQ  VERT  partition    ANOTHER        
  4, Wall-4        OPAQ  VERT  extern_wall  EXTERIOR       
  5, Top-5         OPAQ  CEIL  roof         EXTERIOR       
  6, Base-6        OPAQ  FLOR  grnd_floor   GROUND         
  7, window        TRAN  VERT  window_Not_G EXTERIOR       
# base
  6  0  0  0  0  0    15.00 0
