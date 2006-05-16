# geometry of t_bridge defined in: ../zone/t_bridge.geo
GEN  t_bridge  t_bridge describes a thermal bridge  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     6.33600     4.92500  # vert   1
      3.30000     6.33600     4.92500  # vert   2
      3.30000     6.40000     4.92500  # vert   3
      0.00000     6.40000     4.92500  # vert   4
      0.00000     6.33600     5.10000  # vert   5
      3.30000     6.33600     5.10000  # vert   6
      3.30000     6.40000     5.10000  # vert   7
      0.00000     6.40000     5.10000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  6,  5,
   4,  2,  3,  7,  6,
   4,  3,  4,  8,  7,
   4,  4,  1,  5,  8,
   4,  5,  6,  7,  8,
   4,  1,  4,  3,  2,
# unused index
  0 0 0 0 0 0
# surfaces indentation (m)
 0.000 0.000 0.000 0.000 0.000 0.000
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  mlc db       environment
# no  name         type  posn  name         other side
  1, bed_3_r       OPAQ  VERT  t_bridge_r   bed_3          
  2, bed_2_r       OPAQ  VERT  partition    bed_2          
  3, loft_n_r      OPAQ  VERT  air_gap_ext  loft           
  4, west_ext      OPAQ  VERT  ext_wall_up  EXTERIOR       
  5, loft_up       OPAQ  CEIL  ceil_loft    loft           
  6, bedroom       OPAQ  FLOR  steel_sheet  bed_3_air_gap  
# base
  6  0  0  0  0  0     0.21
