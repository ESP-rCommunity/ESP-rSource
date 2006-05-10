# geometry of office_fl2 defined in: ../zones/office_fl2.geo
GEN  office_fl2             # type   zone name
      12       7   0.000    # vertices, surfaces, rotation angle 
#  X co-ord, Y co-ord, Z co-ord 
      0.00000     0.00000     3.00000  # vert   1
     41.38000     0.00000     3.00000  # vert   2
     41.38000    10.00000     3.00000  # vert   3
      0.00000    10.00000     3.00000  # vert   4
      0.00000     0.00000     7.70000  # vert   5
     41.38000     0.00000     7.70000  # vert   6
     41.38000    10.00000     7.70000  # vert   7
      0.00000    10.00000     7.70000  # vert   8
      1.00000     0.00000     4.00000  # vert   9
     39.38000     0.00000     4.00000  # vert  10
     39.38000     0.00000     5.70000  # vert  11
      1.00000     0.00000     5.70000  # vert  12
# no of vertices followed by list of associated vert
  10,  1,  2,  6,  5,  1,  9, 12, 11, 10,  9,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
   4,  9, 10, 11, 12,
# number of default windows within each surface 
   0  0  0  0  0  0  0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, Surf-1        OPAQ  VERT  wall_clad    EXTERIOR       
  2, Surf-2        OPAQ  VERT  wall_clad    EXTERIOR       
  3, Surf-3        OPAQ  VERT  wall_clad    factory_area   
  4, Surf-4        OPAQ  VERT  wall_clad    EXTERIOR       
  5, Surf-5        OPAQ  CEIL  off_flr-r    office_fl3     
  6, Surf-6        OPAQ  FLOR  off_flr      office_fl1     
  7, window_off2   TRAN  VERT  clear_glaz   EXTERIOR       
# base
   6   0   0   0   413.80
