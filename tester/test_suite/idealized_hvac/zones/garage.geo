# geometry of garage defined in: ../zones/garage.geo
GEN  garage  garage describes the garage with slab on grade floor, exterior s  # type, name, descr
      12       7   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.97000     6.79000     2.60000  # vert   1
     12.00000     6.79000     2.60000  # vert   2
     12.00000    12.20000     2.60000  # vert   3
      4.97000    12.20000     2.60000  # vert   4
      4.97000     6.79000     5.62000  # vert   5
     12.00000     6.79000     5.62000  # vert   6
     12.00000    12.20000     5.62000  # vert   7
      4.97000    12.20000     5.62000  # vert   8
     12.00000     7.07000     2.60000  # vert   9
     12.00000    11.95000     2.60000  # vert  10
     12.00000    11.95000     4.60000  # vert  11
     12.00000     7.07000     4.60000  # vert  12
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   8,  2,  9, 12, 11, 10,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   6,  1,  4,  3, 10,  9,  2,
   4,  9, 10, 11, 12,
# unused index
  0 0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, main_share    OPAQ  VERT  ccht_wall_r  main_second    
  2, front_garage  OPAQ  VERT  ccht_wall    EXTERIOR       
  3, side_wall     OPAQ  VERT  ccht_wall    EXTERIOR       
  4, garage_back   OPAQ  VERT  ccht_wall_r  main_second    
  5, garage_ceiln  OPAQ  CEIL  exp_flr_r    main_second    
  6, slab-on-grad  OPAQ  FLOR  slab_floor   BASESIMP       
  7, garage_door   OPAQ  VERT  ext_doors    EXTERIOR       
# base
   6   0   0   0   0   0    38.03
