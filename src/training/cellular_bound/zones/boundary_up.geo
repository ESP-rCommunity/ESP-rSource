# geometry of boundary_up defined in: ../zones/boundary_up.geo
GEN  boundary_up  boundary_up is a boundary zone above suspended ceiling  # type, name, descr
       8       6   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      0.00000     0.00000     3.50000  # vert   1
      0.00000     5.55000     3.50000  # vert   2
      6.10000     0.00000     3.50000  # vert   3
      6.10000     5.55000     3.50000  # vert   4
      0.00000     0.00000     4.00000  # vert   5
      0.00000     5.55000     4.00000  # vert   6
      6.10000     5.55000     4.00000  # vert   7
      6.10000     0.00000     4.00000  # vert   8
# no of vertices followed by list of associated vert
   4,  1,  2,  4,  3,
   4,  8,  7,  6,  5,
   4,  3,  4,  7,  8,
   4,  4,  2,  6,  7,
   4,  2,  1,  5,  6,
   4,  1,  3,  8,  5,
# unused index
 0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, base          OPAQ  FLOR  structure    ceiling_abv    
  2, t_upper       OPAQ  CEIL  ceiling      CONSTANT       
  3, edg_2upper    OPAQ  VERT  insul_frame  SIMILAR        
  4, edg_3upper    OPAQ  VERT  insul_frame  SIMILAR        
  5, edg_4upper    OPAQ  VERT  insul_frame  SIMILAR        
  6, edg_5upper    OPAQ  VERT  extern_wall  EXTERIOR       
# base
  1  0  0  0  0  0    33.85 0
