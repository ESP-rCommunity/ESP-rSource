# geometry of garage defined in: ../zones/garage.geo
GEN  garage  garage  # type, name, descr
      16       8   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      5.00000     9.00000     0.00000  # vert   1
     10.00000     9.00000     0.00000  # vert   2
     10.00000    15.00000     0.00000  # vert   3
      5.00000    15.00000     0.00000  # vert   4
      5.00000     9.00000     2.70000  # vert   5
     10.00000     9.00000     2.70000  # vert   6
     10.00000    15.00000     2.70000  # vert   7
      5.00000    15.00000     2.70000  # vert   8
      9.50000    15.00000     0.00000  # vert   9
      5.50000    15.00000     0.00000  # vert  10
      5.50000    15.00000     2.40000  # vert  11
      9.50000    15.00000     2.40000  # vert  12
      5.00000     9.50000     0.00000  # vert  13
      5.00000    10.50000     0.00000  # vert  14
      5.00000    10.50000     2.00000  # vert  15
      5.00000     9.50000     2.00000  # vert  16
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   8,  3,  9, 12, 11, 10,  4,  8,  7,
   8,  1,  5,  8,  4, 14, 15, 16, 13,
   4,  5,  6,  7,  8,
   8,  1, 13, 14,  4, 10,  9,  3,  2,
   4,  9, 10, 11, 12,
   4, 14, 13, 16, 15,
# unused index
  0 0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, garage-s      OPAQ  VERT  internl_wal  main           
  2, garage-e      OPAQ  VERT  extern_wall  EXTERIOR       
  3, garage-n      OPAQ  VERT  extern_wall  EXTERIOR       
  4, garage-w      OPAQ  VERT  internl_wal  main           
  5, garage-ceil   OPAQ  CEIL  ceiling      attic          
  6, garage-flor   OPAQ  FLOR  garage-slab  GROUND         
  7, garage-edor   OPAQ  VERT  ext-door     EXTERIOR       
  8, garage-idor   OPAQ  VERT  int_doors    main           
# base
  6  0  0  0  0  0    30.00
